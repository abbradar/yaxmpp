{-# LANGUAGE Strict #-}

module Network.XMPP.Message (
  IMThread (..),
  IMMessage (..),
  plainIMMessage,
  imSlot,
  imSend,
  imPlugin,
)
where

import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.Monad
import Control.Monad.Trans.Except
import Control.Slot (Slot)
import qualified Control.Slot as Slot
import Data.Maybe
import Data.Proxy
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
  , imExtended :: [Element]
  }
  deriving (Show, Eq)

plainIMMessage :: Text -> IMMessage
plainIMMessage txt =
  IMMessage
    { imType = MessageChat
    , imSubject = Nothing
    , imBody = localizedFromText txt
    , imThread = Nothing
    , imExtended = []
    }

type IMSlot m = Slot m (XMPPAddress, IMMessage)

data IMPlugin m = IMPlugin
  { imPluginSlot :: IMSlot m
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
                let imExtended = cur $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement
                return IMMessage {..}

          case res of
            Left e -> return $ InError e
            Right msg -> do
              Slot.call imPluginSlot (from, msg)
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
imSlot = getPluginsHook Proxy

imSend :: (MonadStream m) => XMPPPluginsRef m -> XMPPAddress -> IMMessage -> m ()
imSend pluginsRef to (IMMessage {..}) =
  void $
    stanzaSend
      (pluginsSession pluginsRef)
      OutStanza
        { ostTo = Just to
        , ostType = OutMessage imType
        , ostChildren = subjects ++ bodies ++ maybeToList mThread ++ imExtended
        }
 where
  subjects = maybe [] (localizedElements $ jcName "subject") imSubject
  bodies = localizedElements (jcName "body") imBody
  mThread = fmap (\IMThread {..} -> element (jcName "thread") (maybeToList $ fmap ("parent",) imParent) [NodeContent imId]) imThread

imPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
imPlugin pluginsRef = do
  imPluginSlot <- Slot.new
  let plugin :: IMPlugin m = IMPlugin {..}
  insertPluginsHook imPluginSlot pluginsRef
  inHandlers <- pluginsInHandlers pluginsRef
  HL.pushNewOrFailM plugin inHandlers
