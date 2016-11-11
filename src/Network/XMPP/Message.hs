module Network.XMPP.Message
  ( IMThread(..)
  , IMMessage(..)
  , imSubscribe
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

import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Network.XMPP.XML
import Network.XMPP.Session
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

data IMRef m = IMRef { imSignal :: Signal m (XMPPAddress, IMMessage)
                     , imSession :: StanzaSession m
                     }

imInHandler :: MonadSession m => IMRef m -> InStanza -> m (Maybe (Maybe StanzaError))
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
          Signal.emit imSignal (from, msg)
          return Nothing
        
  where cur = fromChildren istChildren

        getThread e@(Element { elementNodes = [NodeContent imId] }) = return IMThread { imParent = getAttr "parent" e
                                                                                      , ..
                                                                                      }
        getThread _ = Left $ badRequest "getThread: invalid thread element"

imInHandler _ _ = return Nothing

imSubscribe :: MonadSession m => IMRef m -> ((XMPPAddress, IMMessage) -> m ()) -> m ()
imSubscribe (IMRef {..}) = Signal.subscribe imSignal

imSend :: MonadSession m => IMRef m -> XMPPAddress -> IMMessage -> m ()
imSend (IMRef {..}) to (IMMessage {..}) =
  void $ stanzaSend imSession OutStanza { ostTo = Just to
                                        , ostType = OutMessage imType
                                        , ostChildren = subjects ++ bodies ++ maybeToList mThread ++ imExtended
                                        }

  where subjects = maybe [] (localizedElements $ jcName "subject") imSubject
        bodies = localizedElements (jcName "body") imBody
        mThread = fmap (\IMThread {..} -> element (jcName "thread") (maybeToList $ fmap ("parent", ) imParent) [NodeContent imId]) imThread

imPlugin :: MonadSession m => StanzaSession m -> m (XMPPPlugin m, IMRef m)
imPlugin imSession = do
  imSignal <- Signal.empty
  let pref = IMRef {..}
      plugin = XMPPPlugin { pluginInHandler = imInHandler pref
                          , pluginRequestIqHandler = \_ -> return Nothing
                          }
  return (plugin, pref)
