{-# LANGUAGE Strict #-}

module Network.XMPP.Message (
  IMThread (..),
  IMMessage (..),
  plainIMMessage,
  imSlot,
  imCodecs,
  imSend,
  imPlugin,
)
where

import Control.Codec (CodecList, decodeAll, encodeAll)
import qualified Control.Codec as Codec
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Monad.Trans.Except
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Maybe
import Data.Proxy
import Data.Registry (Registry)
import qualified Data.Registry as Reg
import qualified Data.Registry.Mutable as RegRef
import Data.Text (Text)
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XML
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

data IMThread = IMThread
  { imId :: Text
  , imParent :: Maybe Text
  }
  deriving (Show, Eq)

data IMMessage = IMMessage
  { imType :: MessageType
  , imSubject :: Maybe LocalizedText
  , imBody :: LocalizedText
  , imThread :: Maybe IMThread
  , imRaw :: [Element]
  , imExtended :: Registry Show
  }
  deriving (Show)

plainIMMessage :: Text -> IMMessage
plainIMMessage txt =
  IMMessage
    { imType = MessageChat
    , imSubject = Nothing
    , imBody = localizedFromText txt
    , imThread = Nothing
    , imRaw = []
    , imExtended = Reg.empty
    }

type IMSlot m = Slot m (XMPPAddress, IMMessage)

type IMCodecList m = CodecList m XMPPAddress IMMessage

data IMPlugin m = IMPlugin
  { imPluginSlot :: IMSlot m
  , imPluginCodecs :: IMCodecList m
  }

instance (MonadStream m) => Handler m InStanza InResponse (IMPlugin m) where
  tryHandle (IMPlugin {..}) (InStanza {istFrom = Just from, istType = InMessage (Right imType), istChildren})
    | Just bodyRes <- localizedFromElement (jcName "body") istChildren =
        Just <$> do
          let res = runExcept $ do
                let getEither = either throwE return
                imBody <- getEither bodyRes
                imSubject <- mapM getEither $ localizedFromElement (jcName "subject") istChildren
                let thread = listToMaybe $ cur $/ XC.element (jcName "thread") &| curElement
                imThread <- mapM (getEither . getThread) thread
                let imRaw = cur $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement
                    imExtended = Reg.empty
                return IMMessage {..}

          case res of
            Left e -> return $ InError e
            Right msg -> do
              msg' <- decodeAll imPluginCodecs from msg
              Slot.call imPluginSlot (from, msg')
              return InSilent
   where
    cur = fromChildren istChildren

    getThread e@(Element {elementNodes = [NodeContent imId]}) =
      return
        IMThread
          { imParent = getAttr "parent" e
          , ..
          }
    getThread _ = Left $ badRequest "getThread: invalid thread element"
  tryHandle _ _ = return Nothing

-- | Get the IM message slot from the plugins hook set.
imSlot :: (MonadStream m) => XMPPPluginsRef m -> m (IMSlot m)
imSlot = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

-- | Get the IM codec list from the plugins hook set.
imCodecs :: (MonadStream m) => XMPPPluginsRef m -> m (IMCodecList m)
imCodecs = \pluginsRef -> RegRef.lookupOrFailM Proxy $ pluginsHooksSet pluginsRef

imSend :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> IMMessage -> m ()
imSend pluginsRef to msg = do
  codecs <- imCodecs pluginsRef
  IMMessage {..} <- encodeAll codecs to msg
  unless (Reg.null imExtended) $ error "imSend: imExtended is not empty after encoding"
  let subjects = maybe [] (localizedElements $ jcName "subject") imSubject
      bodies = localizedElements (jcName "body") imBody
      mThread = fmap (\IMThread {..} -> element (jcName "thread") (maybeToList $ fmap ("parent",) imParent) [NodeContent imId]) imThread
  void $
    stanzaSend
      (pluginsSession pluginsRef)
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage imType
        , ostChildren = subjects ++ bodies ++ maybeToList mThread ++ imRaw
        }

imPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
imPlugin pluginsRef = do
  imPluginSlot <- Slot.new
  imPluginCodecs <- Codec.new
  let plugin :: IMPlugin m = IMPlugin {..}
  RegRef.insertNewOrFailM imPluginSlot $ pluginsHooksSet pluginsRef
  RegRef.insertNewOrFailM imPluginCodecs $ pluginsHooksSet pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
