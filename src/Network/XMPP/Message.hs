{-# LANGUAGE Strict #-}

module Network.XMPP.Message (
  IMThread (..),
  IMMessage (..),
  AddressedIMMessage (..),
  IMSlot,
  IMCodecList,
  IMPlugin,
  imPluginSlot,
  imPluginCodecs,
  imPluginSelf,
  plainIMMessage,
  getIMPlugin,
  imSend,
  imPlugin,
  parseIMMessage,
)
where

import Control.Codec (CodecList, decodeAll, encodeAll)
import qualified Control.Codec as Codec
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Injective (injFrom, injTo)
import Data.Maybe
import Data.Proxy
import Data.Registry (Registry)
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
import Data.Text (Text)
import qualified Data.UUID as UUID
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Session (sessionAddress)
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XML
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

data IMThread = IMThread
  { imThreadId :: Text
  , imParent :: Maybe Text
  }
  deriving (Show, Eq)

data IMMessage = IMMessage
  { imId :: Maybe MessageId
  , imType :: MessageType
  , imSubject :: Maybe LocalizedText
  , imBody :: LocalizedText
  , imThread :: Maybe IMThread
  , imRaw :: [Element]
  , imExtended :: Registry Show
  }
  deriving (Show)

data AddressedIMMessage = AddressedIMMessage
  { imFrom :: XMPPAddress
  , imTo :: XMPPAddress
  , imMessage :: IMMessage
  }
  deriving (Show)

plainIMMessage :: Text -> IMMessage
plainIMMessage txt =
  IMMessage
    { imId = Nothing
    , imType = MessageChat
    , imSubject = Nothing
    , imBody = localizedFromText txt
    , imThread = Nothing
    , imRaw = []
    , imExtended = Reg.empty
    }

type IMSlot m = Slot m AddressedIMMessage

type IMCodecList m = CodecList m XMPPAddress IMMessage

data IMPlugin m = IMPlugin
  { imPluginSession :: StanzaSession m
  , imPluginSelf :: XMPPAddress
  , imPluginSlot :: IMSlot m
  , imPluginCodecs :: IMCodecList m
  }

{- | Parse a @\<message\>@ element structurally (no codec decoding).
Missing @from@/@to@ default to @self@ per RFC 6120 §8.1.1.1 and §8.1.2.1.
Returns @Right Nothing@ if there is no body, or @Left err@ if malformed.
-}
parseRawIMMessage :: (Monad m) => XMPPAddress -> Element -> m (Either StanzaError (Maybe AddressedIMMessage))
parseRawIMMessage self el = pure $ do
  unless (elementName el == jcName "message") $
    Left $
      badRequest "not a <message> element"
  imType <- case getAttr "type" el of
    Nothing -> Right MessageNormal
    Just t -> case injFrom t of
      Just mt -> Right mt
      Nothing -> Left $ badRequest "invalid message type"
  let imId = getAttr "id" el
  imFrom <- parseAddr "from"
  imTo <- parseAddr "to"
  let children = mapMaybe nodeElement (elementNodes el)
  mBody <- localizedFromElement (jcName "body") children
  case mBody of
    Nothing -> Right Nothing
    Just imBody -> do
      imSubject <- localizedFromElement (jcName "subject") children
      let cur = fromChildren children
          mThreadEl = listToMaybe $ cur $/ XC.element (jcName "thread") &| curElement
      imThread <- traverse parseThread mThreadEl
      let imRaw = cur $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement
          imExtended = Reg.empty
          imMessage = IMMessage {..}
      Right $ Just AddressedIMMessage {imFrom, imTo, imMessage}
 where
  parseAddr attr = case getAttr attr el of
    Nothing -> Right self
    Just txt -> case xmppAddress txt of
      Left _ -> Left $ jidMalformed txt
      Right a -> Right a
  nodeElement (NodeElement ne) = Just ne
  nodeElement _ = Nothing
  parseThread e@(Element {elementNodes = [NodeContent tid]}) =
    Right IMThread {imParent = getAttr "parent" e, imThreadId = tid}
  parseThread _ = Left $ badRequest "invalid <thread> element"

-- | Parse and decode a message through IM codecs.
parseIMMessage :: (MonadStream m) => IMPlugin m -> Element -> m (Either StanzaError (Maybe AddressedIMMessage))
parseIMMessage IMPlugin {imPluginSelf, imPluginCodecs} el = do
  raw <- parseRawIMMessage (addressBare imPluginSelf) el
  case raw of
    Left err -> return $ Left err
    Right Nothing -> return $ Right Nothing
    Right (Just addressed@AddressedIMMessage {imFrom, imMessage = msg}) -> do
      msg' <- decodeAll imPluginCodecs imFrom msg
      return $ Right $ Just $ addressed {imMessage = msg'}

instance (MonadStream m) => Handler m InStanza InResponse (IMPlugin m) where
  tryHandle imp@(IMPlugin {..}) (InStanza {istFrom = Just from, istTo, istId, istType = InMessage (Right imType), istChildren})
    | any ((== jcName "body") . elementName) istChildren =
        Just <$> do
          let attrs =
                ("from", addressToText from)
                  : [("to", addressToText to) | to <- maybeToList istTo]
                  ++ [("id", mid) | mid <- maybeToList istId]
                  ++ [("type", injTo imType)]
              msgEl = element (jcName "message") attrs (map NodeElement istChildren)
          result <- parseIMMessage imp msgEl
          case result of
            Left e -> return $ InError e
            Right Nothing -> return InSilent
            Right (Just addressed) -> do
              Slot.call imPluginSlot addressed
              return InSilent
  tryHandle _ _ = return Nothing

-- | Get the IM plugin from the plugins hook set.
getIMPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (IMPlugin m)
getIMPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (IMPlugin m)) $ pluginsHooksSet pluginsRef

imSend :: (MonadStream m) => IMPlugin m -> XMPPAddress -> IMMessage -> m MessageId
imSend IMPlugin {imPluginSession, imPluginCodecs} to msg = do
  IMMessage {..} <- encodeAll imPluginCodecs to msg
  unless (Reg.null imExtended) $ error "imSend: imExtended is not empty after encoding"
  let subjects = maybe [] (localizedElements $ jcName "subject") imSubject
      bodies = localizedElements (jcName "body") imBody
      mThread = fmap (\IMThread {..} -> element (jcName "thread") (maybeToList $ fmap ("parent",) imParent) [NodeContent imThreadId]) imThread
      out =
        OutStanza
          { ostTo = Just to
          , ostType = OutMessage imType
          , ostChildren = subjects ++ bodies ++ maybeToList mThread ++ imRaw
          }
  case imId of
    Just mid -> stanzaSend' imPluginSession mid out >> return mid
    Nothing -> UUID.toText <$> stanzaSend imPluginSession out

imPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
imPlugin pluginsRef = do
  imPluginSlot <- Slot.new
  imPluginCodecs <- Codec.new
  let imPluginSession = pluginsSession pluginsRef
      imPluginSelf = fullJidAddress $ sessionAddress $ ssSession imPluginSession
      plugin :: IMPlugin m = IMPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsInHandlers pluginsRef
