module Network.XMPP.Stanza
  ( StanzaErrorType(..)
  , StanzaErrorCondition(..)
  , StanzaError(..)
  , badRequest
  , jidMalformed
  , featureNotImplemented

  , MessageType(..)
  , PresenceType(..)
  , OutStanzaType(..)
  , OutStanza(..)
  , InStanzaType(..)
  , InStanza(..)
  , IQRequestType(..)
  , InRequestIQ(..)
  , OutRequestIQ(..)

  , ResponseIQHandler
  , StanzaSession
  , stanzaSend
  , stanzaRequest
  , stanzaSyncRequest

  , InHandler
  , RequestIQHandler
  , stanzaSessionStep

  , stanzaSessionCreate
  , stanzaSessionClose
  , stanzaSessionPeriodic
  ) where

import Text.Read (readMaybe)
import Data.Maybe
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Control.Concurrent.Lifted
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Data.Map (Map)
import qualified Data.Map as M
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import Data.Injective
import Data.ID (IDGen)
import qualified Data.ID as ID
import Network.XMPP.Stream (ClientError)
import Network.XMPP.Session
import Network.XMPP.Address
import Network.XMPP.XML

-- Basic types defined in XMPP CORE and XMPP IM.

data StanzaErrorType = SzAuth
                     | SzCancel
                     | SzContinue
                     | SzModify
                     | SzWait
                     deriving (Show, Eq, Enum, Bounded)

instance Injective StanzaErrorType Text where
  injTo x = case x of
    SzAuth -> "auth"
    SzCancel -> "cancel"
    SzContinue -> "continue"
    SzModify -> "modify"
    SzWait -> "wait"

data StanzaErrorCondition = ScBadRequest
                          | ScConflict
                          | ScFeatureNotImplemented
                          | ScForbidden
                          | ScGone
                          | ScInternalServerError
                          | ScItemNotFound
                          | ScJidMalformed
                          | ScNotAcceptable
                          | ScNotAllowed
                          | ScNotAuthorized
                          | ScPolicyViolation
                          | ScRecipientUnavailable
                          | ScRedirect
                          | ScRegistrationRequired
                          | ScRemoteServerNotFound
                          | ScRemoteServerTimeout
                          | ScResourceConstraint
                          | ScServiceUnavailable
                          | ScUndefinedCondition
                          | ScUnexpectedRequest
                          deriving (Show, Eq, Enum, Bounded)

instance Injective StanzaErrorCondition Text where
  injTo x = case x of
    ScBadRequest -> "bad-request"
    ScConflict -> "conflict"
    ScFeatureNotImplemented -> "feature-not-implemented"
    ScForbidden -> "forbidden"
    ScGone -> "gone"
    ScInternalServerError -> "internal-server-error"
    ScItemNotFound -> "item-not-found"
    ScJidMalformed -> "jid-malformed"
    ScNotAcceptable -> "not-acceptable"
    ScNotAllowed -> "not-allowed"
    ScNotAuthorized -> "not-authorized"
    ScPolicyViolation -> "policy-violation"
    ScRecipientUnavailable -> "recipient-unavailable"
    ScRedirect -> "redirect"
    ScRegistrationRequired -> "registration-required"
    ScRemoteServerNotFound -> "remote-server-not-found"
    ScRemoteServerTimeout -> "remote-server-timeout"
    ScResourceConstraint -> "resource-constraint"
    ScServiceUnavailable -> "service-unavailable"
    ScUndefinedCondition -> "undefined-condition"
    ScUnexpectedRequest -> "unexpected-request"


data StanzaError = StanzaError { szeType :: StanzaErrorType
                               , szeCondition :: StanzaErrorCondition
                               , szeText :: Maybe Text
                               , szeChildren :: [Element]
                               }
                 deriving (Show, Eq)

badRequest :: Text -> StanzaError
badRequest desc = StanzaError { szeType = SzCancel
                              , szeCondition = ScBadRequest
                              , szeText = Just desc
                              , szeChildren = []
                              }

jidMalformed :: Text -> StanzaError
jidMalformed jid = StanzaError { szeType = SzCancel
                               , szeCondition = ScJidMalformed
                               , szeText = Just jid
                               , szeChildren = []
                               }

featureNotImplemented :: Text -> StanzaError
featureNotImplemented desc = StanzaError { szeType = SzCancel
                                         , szeCondition = ScFeatureNotImplemented
                                         , szeText = Just desc
                                         , szeChildren = []
                                         }

data MessageType = MessageChat
                 | MessageGroupchat
                 | MessageHeadline
                 | MessageNormal
                 deriving (Show, Eq, Enum, Bounded)

instance Injective MessageType Text where
  injTo x = case x of
    MessageChat -> "chat"
    MessageGroupchat -> "groupchat"
    MessageHeadline -> "headline"
    MessageNormal -> "normal"

data PresenceType = PresenceProbe
                  | PresenceSubscribe
                  | PresenceSubscribed
                  | PresenceUnavailable
                  | PresenceUnsubscribe
                  | PresenceUnsubscribed
                  deriving (Show, Eq, Enum, Bounded)

instance Injective PresenceType Text where
  injTo x = case x of
    PresenceProbe -> "probe"
    PresenceSubscribe -> "subscribe"
    PresenceSubscribed -> "subscribed"
    PresenceUnavailable -> "unavailable"
    PresenceUnsubscribe -> "unsubscribe"
    PresenceUnsubscribed -> "unsubscribed"

data OutStanzaType = OutMessage MessageType
                   | OutPresence PresenceType
                   deriving (Show, Eq)

data OutStanza = OutStanza { ostTo :: Maybe XMPPAddress
                           , ostType :: OutStanzaType
                           }
               deriving (Show, Eq)

data InStanzaType = InMessage (Either StanzaError MessageType)
                  | InPresence (Either StanzaError PresenceType)
                  deriving (Show, Eq)

data InStanza = InStanza { istFrom :: Maybe XMPPAddress
                         , istTo :: Maybe XMPPAddress
                         , istId :: Maybe Text
                         , istType :: InStanzaType
                         , istChildren :: [Element]
                         }
              deriving (Show, Eq)


data IQRequestType = IQGet
                   | IQSet
                   deriving (Show, Eq, Enum, Bounded)

instance Injective IQRequestType Text where
  injTo x = case x of
    IQGet -> "get"
    IQSet -> "set"

data InRequestIQ = InRequestIQ { iriFrom :: Maybe XMPPAddress
                               , iriTo :: Maybe XMPPAddress
                               , iriId :: Text
                               , iriIqType :: IQRequestType
                               , iriChildren :: [Element]
                               }
                 deriving (Show, Eq)

data OutRequestIQ = OutRequestIQ { oriTo :: Maybe XMPPAddress
                                 , oriIqType :: IQRequestType
                                 , oriChildren :: [Element]
                                 }
                  deriving (Show, Eq)

stanzaName :: T.Text -> Name
stanzaName = nsName "urn:ietf:params:xml:ns:xmpp-stanzas"


type ResponseIQHandler m = Either (StanzaError, [Element]) [Element] -> m ()

data StanzaSession m = StanzaSession { ssSession :: Session m
                                     , ssReqIds :: MVar IDGen
                                     , ssRequests :: MVar (Map Integer (ResponseIQHandler m))
                                     }

stanzaSend :: MonadSession m => StanzaSession m -> OutStanza -> [Element] -> m Integer
stanzaSend (StanzaSession {..}) (OutStanza {..}) body = modifyMVar ssReqIds $ \idgen -> do
  let (idgen', sid) = ID.get idgen
      (mname, mtype) = case ostType of
        OutMessage t -> ("message", injTo t)
        OutPresence t -> ("presence", injTo t)
      attrs = [ ("id", T.pack $ show sid)
              , ("type", mtype)
              ] ++ maybeToList (fmap (("to", ) . showXMPPAddress) ostTo)
      msg = element (jcName mname) attrs $ map NodeElement body
  sessionSend ssSession msg
  return (idgen', sid)

stanzaRequest :: MonadSession m => StanzaSession m -> OutRequestIQ -> ResponseIQHandler m -> m ()
stanzaRequest (StanzaSession {..}) (OutRequestIQ {..}) handler = modifyMVar_ ssReqIds $ \idgen -> modifyMVar ssRequests $ \reqs -> do
  let (idgen', sid) = ID.get idgen
      attrs = [ ("id", T.pack $ show sid)
              , ("type", injTo oriIqType)
              ] ++ maybeToList (fmap (("to", ) . showXMPPAddress) oriTo)
      msg = element "iq" attrs $ map NodeElement oriChildren
  sessionSend ssSession msg
  return (M.insert sid handler reqs, idgen')

stanzaSyncRequest :: MonadSession m => StanzaSession m -> OutRequestIQ -> m (Either (StanzaError, [Element]) [Element])
stanzaSyncRequest session req = do
  ret <- newEmptyMVar
  stanzaRequest session req $ \res -> putMVar ret res
  takeMVar ret

stanzaSendError :: MonadSession m => StanzaSession m -> Element -> StanzaError -> m ()
stanzaSendError (StanzaSession {..}) e (StanzaError {..}) =
  sessionSend ssSession $ element (elementName e) (("type", "error") : catMaybes attrs) (elementNodes e ++ [errorE])

  where attrs = [ ("id", ) <$> getAttr "id" e
                , ("to", ) <$> getAttr "from" e
                , ("from", ) <$> getAttr "to" e
                ]
        errorE = NodeElement $ element "error" [("type", injTo szeType)] $
                 [ NodeElement $ closedElement $ stanzaName $ injTo $ szeCondition 
                 ]
                 ++ maybeToList (fmap (\t -> NodeElement $ element (stanzaName "text") [(xmlName "lang", "en")] [NodeContent t]) szeText)
                 ++ map NodeElement szeChildren

type InHandler m = InStanza -> m (Maybe StanzaError)
type RequestIQHandler m = InRequestIQ -> m (Either StanzaError [Element])

getStanzaError :: Element -> (StanzaError, [Element])
getStanzaError e = (StanzaError {..}, others)

  where cur = fromNode $ NodeElement e

        szeType = fromMaybe SzCancel $ do
          ttype <- listToMaybe $ cur $/ XC.element "error" &/ attribute "type"
          injFrom ttype

        szeCondition = fromMaybe ScUndefinedCondition $ do
          NodeElement en <- listToMaybe $ cur $/ XC.element "error" &/ anyElement &| node
          injFrom $ nameLocalName $ elementName en

        szeText = listToMaybe $ cur $/ XC.element "error" &/ XC.element (stanzaName "text") &/ content

        szeChildren = map (\(node -> NodeElement ec) -> ec) $ cur $/ XC.element "error" &/ checkName (\n -> n /= stanzaName (injTo szeCondition) && n /= stanzaName "text")

        others = map (\(node -> NodeElement ce) -> ce) $ cur $/ checkName (/= "error")

checkOrFail :: Monad m => Maybe a -> m () -> MaybeT m a
checkOrFail Nothing finalize = MaybeT $ finalize >> return Nothing
checkOrFail (Just a) _ = MaybeT $ return $ Just a

stanzaSessionStep :: MonadSession m => StanzaSession m -> InHandler m -> RequestIQHandler m -> m ()
stanzaSessionStep sess@(StanzaSession {..}) inHandler reqHandler = void $ runMaybeT $ do
  e <- MaybeT $ sessionStep ssSession

  let sendError = stanzaSendError sess e
      getAddr name = mapM extractAddr $ getAttr name e
        where extractAddr addr = checkOrFail (readXMPPAddress addr) $ sendError $ jidMalformed addr

      ename = elementName e
      payload = mapMaybe (\case NodeElement ne -> Just ne; _ -> Nothing) $ elementNodes e

  ttype <- checkOrFail (getAttr "type" e) $ sendError $ badRequest "stanzaSessionStep: message type is not specified"
  tfrom <- getAddr "from"
  tto <- getAddr "to"

  if | ename == jcName "iq" -> do
         mid <- checkOrFail (getAttr "id" e) $ sendError $ badRequest "stanzaSessionStep: iq id is not specified"
         case injFrom ttype of
           Just rtype -> lift $ do
             res <- reqHandler InRequestIQ { iriFrom = tfrom
                                          , iriTo = tto
                                          , iriId = mid
                                          , iriIqType = rtype
                                          , iriChildren = payload
                                          }
             case res of
               Left serr -> sendError serr
               Right cres -> do
                 let attrs =
                       [ ("id", mid)
                       , ("type", "result")
                       ] ++ catMaybes [ ("to", ) <$> getAttr "from" e
                                      , ("from", ) <$> getAttr "to" e
                                      ]
                 sessionSend ssSession $ element (jcName "iq") attrs $ map NodeElement cres

           Nothing -> do
             nid <- checkOrFail (readMaybe $ T.unpack mid) $ sendError $ badRequest "stanzaSessionStep: iq response id is invalid"
             lift $ modifyMVar_ ssRequests $ \requests ->
               case M.lookup nid requests of
                 Nothing -> do
                   sendError $ badRequest "stanzaSessionStep: corresponding request for response is not found"
                   return requests
                 Just handler -> do
                   if | ttype == "result" -> handler $ Right payload
                      | ttype == "error" -> handler $ Left $ getStanzaError e
                      | otherwise -> sendError $ badRequest "stanzaSessionStep: iq type is invalid"
                   return $ M.delete nid requests

     | otherwise -> do
         let (serror, children) =
               if ttype == "error"
               then let (err, ch) = getStanzaError e in (Just err, ch)
               else (Nothing, payload)
         mtype <-
           if | ename == jcName "message" -> do
                  let getType = checkOrFail (injFrom ttype) $ sendError $ badRequest "stanzaSessionStep: invalid message type"
                  InMessage <$> maybe (Right <$> getType) (return . Left) serror
              | ename == jcName "presence" -> do
                  let getType = checkOrFail (injFrom ttype) $ sendError $ badRequest "stanzaSessionStep: invalid presence type"
                  InPresence <$> maybe (Right <$> getType) (return . Left) serror
              | otherwise -> do
                  lift $ sendError $ badRequest "stanzaSessionStep: unknown stanza type"
                  mzero

         res <- MaybeT $ inHandler $ InStanza { istFrom = tfrom
                                             , istTo = tto
                                             , istId = getAttr "id" e
                                             , istType = mtype
                                             , istChildren = children
                                             }
         lift $ sendError res

stanzaSessionCreate :: MonadSession m => SessionSettings -> m (Either ClientError (StanzaSession m))
stanzaSessionCreate sessionSettings = runEitherT $ do
  ssSession <- EitherT $ sessionCreate sessionSettings
  ssReqIds <- lift $ newMVar ID.empty
  ssRequests <- lift $ newMVar M.empty
  return StanzaSession {..}

stanzaSessionClose :: MonadSession m => StanzaSession m -> m () 
stanzaSessionClose (StanzaSession {..}) = sessionClose ssSession

stanzaSessionPeriodic :: MonadSession m => StanzaSession m -> m () 
stanzaSessionPeriodic (StanzaSession {..}) = sessionPeriodic ssSession
