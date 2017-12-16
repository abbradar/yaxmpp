module Network.XMPP.Stanza
  ( StanzaErrorType(..)
  , StanzaErrorCondition(..)
  , StanzaError(..)
  , badRequest
  , jidMalformed
  , featureNotImplemented
  , serviceUnavailable
  , itemNotFound

  , MessageType(..)
  , PresenceType(..)
  , OutStanzaType(..)
  , OutStanza(..)
  , InStanzaType(..)
  , InStanza(..)
  , IQRequestType(..)
  , InRequestIQ(..)
  , OutRequestIQ(..)
  , serverRequest

  , ResponseIQHandler
  , StanzaSession
  , ssSession
  , stanzaSend
  , stanzaSend'
  , stanzaRequest
  , stanzaSyncRequest

  , InHandler
  , RequestIQHandler
  , stanzaSessionStep

  , stanzaSessionCreate
  ) where

import Data.Maybe
import Control.Monad
import Data.Text (Text)
import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Logger
import Data.Map (Map)
import qualified Data.Map as M
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Text.InterpolatedString.Perl6 (qq)
import Data.Default.Class
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID

import Data.Injective
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Address
import Network.XMPP.Utils
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

instance Default StanzaError where
  def = StanzaError { szeType = SzCancel
                    , szeCondition = ScUndefinedCondition
                    , szeText = Nothing
                    , szeChildren = []
                    }

badRequest :: Text -> StanzaError
badRequest desc = def { szeType = SzModify
                      , szeCondition = ScBadRequest
                      , szeText = Just desc
                      }

jidMalformed :: Text -> StanzaError
jidMalformed jid = def { szeType = SzModify
                       , szeCondition = ScJidMalformed
                       , szeText = Just jid
                       }

featureNotImplemented :: Text -> StanzaError
featureNotImplemented desc = def { szeCondition = ScFeatureNotImplemented
                                 , szeText = Just desc
                                 }

serviceUnavailable :: Text -> StanzaError
serviceUnavailable desc = def { szeCondition = ScServiceUnavailable
                              , szeText = Just desc
                              }

itemNotFound :: Text -> StanzaError
itemNotFound desc = def { szeCondition = ScItemNotFound
                        , szeText = Just desc
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
                   | OutPresence (Maybe PresenceType)
                   deriving (Show, Eq)

data OutStanza = OutStanza { ostTo :: Maybe XMPPAddress
                           , ostType :: OutStanzaType
                           , ostChildren :: [Element]
                           }
               deriving (Show, Eq)

data InStanzaType = InMessage (Either StanzaError MessageType)
                  | InPresence (Either StanzaError (Maybe PresenceType))
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
                               , iriType :: IQRequestType
                               , iriChildren :: [Element]
                               }
                 deriving (Show, Eq)

data IQResponseType = IQResult
                    | IQError
                    deriving (Show, Eq, Enum, Bounded)

instance Injective IQResponseType Text where
  injTo x = case x of
    IQResult -> "result"
    IQError -> "error"

data OutRequestIQ = OutRequestIQ { oriTo :: Maybe XMPPAddress
                                 , oriIqType :: IQRequestType
                                 , oriChildren :: [Element]
                                 }
                  deriving (Show, Eq)

serverRequest :: IQRequestType -> [Element] -> OutRequestIQ
serverRequest oriIqType oriChildren = OutRequestIQ { oriTo = Nothing
                                                   , ..
                                                   }

stanzaName :: Text -> Name
stanzaName = nsName "urn:ietf:params:xml:ns:xmpp-stanzas"


type ResponseIQHandler m = Either (StanzaError, [Element]) [Element] -> m ()

data StanzaSession m = StanzaSession { ssSession :: Session m
                                     , ssRequests :: MVar (Map (Maybe XMPPAddress, UUID) (ResponseIQHandler m))
                                     }

stanzaSend :: MonadStream m => StanzaSession m -> OutStanza ->  m UUID
stanzaSend sess stanza = do
  sid <- liftIO $ UUID.nextRandom
  stanzaSend' sess (UUID.toText sid) stanza
  return sid

stanzaSend' :: MonadStream m => StanzaSession m -> Text -> OutStanza -> m ()
stanzaSend' (StanzaSession {..}) sid (OutStanza {..}) = do
  let (mname, mtype) = case ostType of
        OutMessage t -> ("message", Just $ injTo t)
        OutPresence t -> ("presence", injTo <$> t)
      attrs = [ ("id", sid)
              ] ++ maybeToList (fmap (("to", ) . addressToText) ostTo)
                ++ maybeToList (fmap ("type", ) mtype)
      msg = element (jcName mname) attrs $ map NodeElement ostChildren
  sessionSend ssSession msg

stanzaRequest :: MonadStream m => StanzaSession m -> OutRequestIQ -> ResponseIQHandler m -> m ()
stanzaRequest (StanzaSession {..}) (OutRequestIQ {..}) handler = modifyMVar_ ssRequests $ \reqs -> do
  let findSid = do
        sid <- liftIO $ UUID.nextRandom
        if (oriTo, sid) `M.member` reqs
          then findSid
          else return sid
  sid <- findSid
  let attrs = [ ("id", UUID.toText sid)
              , ("type", injTo oriIqType)
              ] ++ maybeToList (fmap (("to", ) . addressToText) oriTo)
      msg = element (jcName "iq") attrs $ map NodeElement oriChildren
  sessionSend ssSession msg
  return $ M.insert (oriTo, sid) handler reqs

stanzaSyncRequest :: MonadStream m => StanzaSession m -> OutRequestIQ -> m (Either (StanzaError, [Element]) [Element])
stanzaSyncRequest session req = do
  ret <- newEmptyMVar
  stanzaRequest session req $ \res -> putMVar ret res
  takeMVar ret

stanzaSendError :: MonadStream m => StanzaSession m -> Element -> StanzaError -> m ()
stanzaSendError (StanzaSession {..}) e err@(StanzaError {..}) = do
  $(logWarn) [qq|Stanza error sent: $err|]
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
  where topCur = fromElement e
        errorE = case topCur $/ XC.element (jcName "error") &| curElement of
          (err:_) -> err
          _ -> closedElement "error"
        cur = fromElement errorE

        szeType = fromMaybe SzCancel $ getAttr "type" errorE >>= injFrom

        szeCondition = fromMaybe ScUndefinedCondition $ do
          en <- listToMaybe $ cur $/ curAnyElement
          injFrom $ nameLocalName $ elementName en

        szeText = listToMaybe $ cur $/ XC.element (stanzaName "text") &/ content

        szeChildren = cur $/ checkName (\n -> n /= stanzaName (injTo szeCondition) && n /= stanzaName "text") &| curElement

        others = topCur $/ checkName (/= jcName "error") &| curElement

checkOrFail :: Monad m => Maybe a -> m () -> MaybeT m a
checkOrFail Nothing finalize = MaybeT $ finalize >> return Nothing
checkOrFail (Just a) _ = MaybeT $ return $ Just a

stanzaSessionStep :: MonadStream m => StanzaSession m -> InHandler m -> RequestIQHandler m -> m ()
stanzaSessionStep sess@(StanzaSession {..}) inHandler reqHandler = void $ runMaybeT $ do
  e <- MaybeT $ sessionStep ssSession

  let sendError = stanzaSendError sess e
      sendErrorOnIq err
        | ename == jcName "iq" = sendError err
        | otherwise = $(logWarn) [qq|Error in received non-IQ stanza: $err|]
      getAddr name = mapM extractAddr $ getAttr name e
        where extractAddr addr = checkOrFail (parseValue xmppAddress addr) $ sendErrorOnIq $ jidMalformed [qq|stanzaSessionStep: malformed address $addr|]

      ename = elementName e
      payload = mapMaybe (\case NodeElement ne -> Just ne; _ -> Nothing) $ elementNodes e

  let tmtype = getAttr "type" e
  let tmid = getAttr "id" e
  tfrom' <- getAddr "from"
  -- Workaround for ejabberd that sometimes uses bare JID as "from" for server responses.
  let bare = bareJidAddress $ fullBare $ sessionAddress ssSession
      tfrom = tfrom' >>= \from -> if from == bare then Nothing else return from
  tto <- getAddr "to"

  if | ename == jcName "iq" -> do
         ttype <- checkOrFail tmtype $ sendError $ badRequest "stanzaSessionStep: iq type is not specified"
         tid <- checkOrFail tmid $ sendError $ badRequest "stanzaSessionStep: iq id is not specified"
         case injFrom ttype of
           Just reqType -> lift $ do
             res <- reqHandler InRequestIQ { iriFrom = tfrom
                                          , iriTo = tto
                                          , iriId = tid
                                          , iriType = reqType
                                          , iriChildren = payload
                                          }
             case res of
               Left serr -> sendError serr
               Right cres -> do
                 let attrs =
                       [ ("id", tid)
                       , ("type", "result")
                       ] ++ catMaybes [ ("to", ) <$> getAttr "from" e
                                      , ("from", ) <$> getAttr "to" e
                                      ]
                 sessionSend ssSession $ element (jcName "iq") attrs $ map NodeElement cres
           Nothing -> case injFrom ttype of
             Just resType -> do
               nid <- checkOrFail (UUID.fromText tid) $ sendError $ badRequest "stanzaSessionStep: iq response id is invalid"
               lift $ do
                 mhandler <- modifyMVar ssRequests $ \requests -> case M.lookup (tfrom, nid) requests of
                   Nothing -> return (requests, Nothing)
                   Just handler -> return (M.delete (tfrom, nid) requests, Just handler)
                 case (mhandler, resType) of
                   (Nothing, _) -> sendError $ badRequest "stanzaSessionStep: corresponding request for response is not found"
                   (Just handler, IQResult) -> handler $ Right payload
                   (Just handler, IQError) -> handler $ Left $ getStanzaError e
             Nothing -> lift $ sendError $ badRequest "stanzaSessionStep: iq type is invalid"


     | otherwise -> do
         let (ttype, children) =
               if tmtype == Just "error"
               then let (err, ch) = getStanzaError e in (Left err, ch)
               else (Right tmtype, payload)
         mtype <-
           if | ename == jcName "message" -> do
                  let getType mt = case mt of
                        Nothing -> return MessageNormal
                        Just t -> checkOrFail (injFrom t) $ sendErrorOnIq $ badRequest "stanzaSessionStep: invalid message type"
                  InMessage <$> mapM getType ttype
              | ename == jcName "presence" -> do
                  let getType t = checkOrFail (injFrom t) $ sendErrorOnIq $ badRequest "stanzaSessionStep: invalid presence type"
                  InPresence <$> mapM (mapM getType) ttype
              | otherwise -> do
                  lift $ sendErrorOnIq $ badRequest "stanzaSessionStep: unknown stanza type"
                  mzero

         res <- MaybeT $ inHandler $ InStanza { istFrom = tfrom
                                             , istTo = tto
                                             , istId = tmid
                                             , istType = mtype
                                             , istChildren = children
                                             }
         lift $ sendError res

stanzaSessionCreate :: MonadStream m => Session m -> m (StanzaSession m)
stanzaSessionCreate ssSession = do
  ssRequests <- newMVar M.empty
  return StanzaSession {..}
