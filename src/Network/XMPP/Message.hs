{-# LANGUAGE Strict #-}

module Network.XMPP.Message
  ( IMThread(..)
  , IMMessage(..)
  , plainIMMessage
  , imSlot
  , imSend
  , imPlugin
  ) where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Except
import Data.Text (Text)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Control.Slot (Slot, SlotRef)
import qualified Control.Slot as Slot
import qualified Control.HandlerList as HandlerList
import Network.XMPP.XML
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Language
import Network.XMPP.Address
import Network.XMPP.Plugin

data IMThread = IMThread { imId :: Text
                         , imParent :: Maybe Text
                         }
              deriving (Show, Eq)

data IMMessage = IMMessage { imType :: MessageType
                           , imSubject :: Maybe LocalizedText
                           , imBody :: LocalizedText
                           , imThread :: Maybe IMThread
                           , imExtended :: [Element]
                           }
               deriving (Show, Eq)

plainIMMessage :: Text -> IMMessage
plainIMMessage txt = IMMessage { imType = MessageChat
                               , imSubject = Nothing
                               , imBody = localizedFromText txt
                               , imThread = Nothing
                               , imExtended = []
                               }

data IMRef m = IMRef { imSlotI :: Slot m (XMPPAddress, IMMessage)
                     , imSession :: StanzaSession m
                     }

imInHandler :: MonadStream m => IMRef m -> InStanza -> m (Maybe InResponse)
imInHandler (IMRef {..}) (InStanza { istFrom = Just from, istType = InMessage (Right imType), istChildren })
  | Just bodyRes <- localizedFromElement (jcName "body") istChildren = Just <$> do
      let res = runExcept $ do
            let getEither = either throwE return
            imBody <- getEither bodyRes
            imSubject <- mapM getEither $ localizedFromElement (jcName "subject") istChildren
            let thread = listToMaybe $ cur $/ XC.element (jcName "thread") &| curElement
            imThread <- mapM (getEither . getThread) thread
            let imExtended = cur $/ checkName ((/= Just jcNS) . nameNamespace) &| curElement
            return IMMessage {..}

      case res of
        Left e -> return $ Just e
        Right msg -> do
          Slot.call imSlotI (from, msg)
          return Nothing
        
  where cur = fromChildren istChildren

        getThread e@(Element { elementNodes = [NodeContent imId] }) = return IMThread { imParent = getAttr "parent" e
                                                                                      , ..
                                                                                      }
        getThread _ = Left $ badRequest "getThread: invalid thread element"

imInHandler _ _ = return Nothing

imSlot :: MonadStream m => IMRef m -> SlotRef m (XMPPAddress, IMMessage)
imSlot (IMRef {..}) = Slot.ref imSlotI

imSend :: MonadStream m => IMRef m -> XMPPAddress -> IMMessage -> m ()
imSend (IMRef {..}) to (IMMessage {..}) =
  void $ stanzaSend imSession OutStanza { ostTo = Just to
                                        , ostType = OutMessage imType
                                        , ostChildren = subjects ++ bodies ++ maybeToList mThread ++ imExtended
                                        }

  where subjects = maybe [] (localizedElements $ jcName "subject") imSubject
        bodies = localizedElements (jcName "body") imBody
        mThread = fmap (\IMThread {..} -> element (jcName "thread") (maybeToList $ fmap ("parent", ) imParent) [NodeContent imId]) imThread

imPlugin :: MonadStream m => XMPPPluginsRef m -> m (IMRef m)
imPlugin pluginsRef = do
  imSlotI <- Slot.new
  let pref = IMRef { imSession = pluginsSession pluginsRef, .. }
  void $ HandlerList.add (pluginInHandlers pluginsRef) $ imInHandler pref
  return pref
