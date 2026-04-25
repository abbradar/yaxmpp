{-# LANGUAGE Strict #-}

module Network.XMPP.Stanza (
  StanzaErrorType (..),
  StanzaErrorCondition (..),
  StanzaError (..),
  badRequest,
  jidMalformed,
  featureNotImplemented,
  serviceUnavailable,
  itemNotFound,
  StanzaId,
  MessageType (..),
  PresenceType (..),
  OutStanzaType (..),
  OutStanza (..),
  InStanzaType (..),
  InStanza (..),
  IQRequestType (..),
  InRequestIQ (..),
  OutRequestIQ (..),
  serverRequest,
  fromServerOrMyself,
  IQResponse,
  IQResponseHandler,
  StanzaSession,
  ssSession,
  stanzaSend,
  stanzaSend',
  stanzaRequest,
  parseInStanza,
  parseStanzaError,
  InResponse (..),
  InHandler,
  RequestIQResponse (..),
  IQHandler,
  SessionHooks (..),
  stanzaSessionStep,
  stanzaSessionCreate,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import UnliftIO.IORef

import Data.Injective
import Network.XMPP.Address
import Network.XMPP.Session
import Network.XMPP.Stream
import Network.XMPP.Utils
import Network.XMPP.XML

-- Basic types defined in XMPP CORE and XMPP IM.

data StanzaErrorType
  = SzAuth
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

data StanzaErrorCondition
  = ScBadRequest
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

data StanzaError = StanzaError
  { szeType :: StanzaErrorType
  , szeCondition :: StanzaErrorCondition
  , szeText :: Maybe Text
  , szeChildren :: [Element]
  }
  deriving (Show, Eq)

undefinedError :: StanzaError
undefinedError =
  StanzaError
    { szeType = SzCancel
    , szeCondition = ScUndefinedCondition
    , szeText = Nothing
    , szeChildren = []
    }

badRequest :: Text -> StanzaError
badRequest desc =
  undefinedError
    { szeType = SzModify
    , szeCondition = ScBadRequest
    , szeText = Just desc
    }

jidMalformed :: Text -> StanzaError
jidMalformed jid =
  undefinedError
    { szeType = SzModify
    , szeCondition = ScJidMalformed
    , szeText = Just jid
    }

featureNotImplemented :: Text -> StanzaError
featureNotImplemented desc =
  undefinedError
    { szeCondition = ScFeatureNotImplemented
    , szeText = Just desc
    }

serviceUnavailable :: Text -> StanzaError
serviceUnavailable desc =
  undefinedError
    { szeCondition = ScServiceUnavailable
    , szeText = Just desc
    }

itemNotFound :: Text -> StanzaError
itemNotFound desc =
  undefinedError
    { szeCondition = ScItemNotFound
    , szeText = Just desc
    }

-- | A stanza @id@ attribute value.
type StanzaId = Text

data MessageType
  = MessageChat
  | MessageGroupchat
  | MessageHeadline
  | MessageNormal
  | MessageError
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Injective MessageType Text where
  injTo x = case x of
    MessageChat -> "chat"
    MessageGroupchat -> "groupchat"
    MessageHeadline -> "headline"
    MessageNormal -> "normal"
    MessageError -> "error"

data PresenceType
  = PresenceProbe
  | PresenceSubscribe
  | PresenceSubscribed
  | PresenceUnavailable
  | PresenceUnsubscribe
  | PresenceUnsubscribed
  | PresenceError
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Injective PresenceType Text where
  injTo x = case x of
    PresenceProbe -> "probe"
    PresenceSubscribe -> "subscribe"
    PresenceSubscribed -> "subscribed"
    PresenceUnavailable -> "unavailable"
    PresenceUnsubscribe -> "unsubscribe"
    PresenceUnsubscribed -> "unsubscribed"
    PresenceError -> "error"

data OutStanzaType
  = OutMessage MessageType
  | OutPresence (Maybe PresenceType)
  deriving (Show, Eq)

data OutStanza = OutStanza
  { ostTo :: Maybe XMPPAddress
  , ostType :: OutStanzaType
  , ostChildren :: [Element]
  }
  deriving (Show, Eq)

data InStanzaType
  = InMessage MessageType
  | InPresence (Maybe PresenceType)
  deriving (Show, Eq, Ord)

data InStanza = InStanza
  { istFrom :: Maybe XMPPAddress
  , istTo :: Maybe XMPPAddress
  , istId :: Maybe StanzaId
  , istType :: InStanzaType
  , istChildren :: [Element]
  }
  deriving (Show, Eq)

data IQRequestType
  = IQGet
  | IQSet
  deriving (Show, Eq, Enum, Bounded)

instance Injective IQRequestType Text where
  injTo x = case x of
    IQGet -> "get"
    IQSet -> "set"

data InRequestIQ = InRequestIQ
  { iriFrom :: Maybe XMPPAddress
  , iriTo :: Maybe XMPPAddress
  , iriId :: StanzaId
  , iriType :: IQRequestType
  , iriChildren :: [Element]
  }
  deriving (Show, Eq)

data IQResultType
  = IQTypeResult
  | IQTypeError
  deriving (Show, Eq, Enum, Bounded)

instance Injective IQResultType Text where
  injTo x = case x of
    IQTypeResult -> "result"
    IQTypeError -> "error"

data OutRequestIQ = OutRequestIQ
  { oriTo :: Maybe XMPPAddress
  , oriIqType :: IQRequestType
  , oriChildren :: [Element]
  }
  deriving (Show, Eq)

serverRequest :: IQRequestType -> [Element] -> OutRequestIQ
serverRequest oriIqType oriChildren =
  OutRequestIQ
    { oriTo = Nothing
    , ..
    }

stanzaName :: Text -> Name
stanzaName = nsName "urn:ietf:params:xml:ns:xmpp-stanzas"

type IQResponse = Either StanzaError [Element]

type IQResponseHandler m = IQResponse -> m ()

data StanzaSession m = StanzaSession
  { ssSession :: Session m
  , ssRequests :: IORef (Map (Maybe XMPPAddress, StanzaId) (IQResponseHandler m))
  , ssNextIqId :: IORef Word
  }

fromServerOrMyself :: Maybe XMPPAddress -> StanzaSession m -> Bool
fromServerOrMyself Nothing _ = True
fromServerOrMyself (Just XMPPAddress {..}) sess = addressDomain == bareDomain (fullBare myAddress) && localOk && resourceOk
 where
  myAddress = sessionAddress $ ssSession sess
  localOk =
    case addressLocal of
      Nothing -> True
      Just local -> local == bareLocal (fullBare myAddress)
  resourceOk =
    case addressResource of
      Nothing -> True
      Just resource -> resource == fullResource myAddress

stanzaSend :: (MonadStream m) => StanzaSession m -> OutStanza -> m UUID
stanzaSend sess stanza = do
  sid <- liftIO UUID.nextRandom
  stanzaSend' sess (UUID.toText sid) stanza
  return sid

stanzaSend' :: (MonadStream m) => StanzaSession m -> StanzaId -> OutStanza -> m ()
stanzaSend' StanzaSession {..} sid OutStanza {..} = do
  let (mname, mtype) = case ostType of
        OutMessage t -> ("message", Just $ injTo t)
        OutPresence t -> ("presence", injTo <$> t)
      attrs =
        [ ("id", sid)
        ]
          ++ maybeToList (fmap (("to",) . addressToText) ostTo)
          ++ maybeToList (fmap ("type",) mtype)
      msg = element (jcName mname) attrs $ map NodeElement ostChildren
  sessionSend ssSession msg

-- Per RFC 6120 §10.3.3, server-on-behalf-of-account IQs use the
-- account's bare JID as @from@/@to@; that is equivalent to absent.
-- Normalize for the lookups.
normalizeIQEntity :: Session m -> Maybe XMPPAddress -> Maybe XMPPAddress
normalizeIQEntity ssSession (Just addr)
  | addr == bare = Nothing
  | otherwise = Just addr
 where
  bare = bareJidAddress $ fullBare $ sessionAddress ssSession
normalizeIQEntity _ Nothing = Nothing

stanzaRequest :: (MonadStream m) => StanzaSession m -> OutRequestIQ -> IQResponseHandler m -> m ()
stanzaRequest StanzaSession {..} OutRequestIQ {..} handler = do
  sid <- atomicModifyIORef' ssNextIqId $ \n -> (n + 1, T.pack (show n))
  let oriTo' = normalizeIQEntity ssSession oriTo
  atomicModifyIORef' ssRequests $ \reqs -> (M.insert (oriTo', sid) handler reqs, ())
  let attrs =
        [ ("id", sid)
        , ("type", injTo oriIqType)
        ]
          ++ maybeToList (fmap (("to",) . addressToText) oriTo')
      msg = element (jcName "iq") attrs $ map NodeElement oriChildren
  sessionSend ssSession msg

stanzaSendError :: (MonadStream m) => StanzaSession m -> Element -> StanzaError -> m ()
stanzaSendError StanzaSession {..} e err@StanzaError {..} = do
  $(logWarn) [i|Stanza error sent: #{err}|]
  sessionSend ssSession $ element (elementName e) (("type", "error") : catMaybes attrs) (elementNodes e ++ [errorE])
 where
  attrs =
    [ ("id",) <$> getAttr "id" e
    , ("to",) <$> getAttr "from" e
    , ("from",) <$> getAttr "to" e
    ]
  errorE =
    NodeElement $
      element "error" [("type", injTo szeType)] $
        [ NodeElement $ closedElement $ stanzaName $ injTo szeCondition
        ]
          ++ maybeToList (fmap (\t -> NodeElement $ element (stanzaName "text") [(xmlName "lang", "en")] [NodeContent t]) szeText)
          ++ map NodeElement szeChildren

data InResponse
  = InSilent
  | InError StanzaError
  deriving (Show, Eq)

type InHandler m = InStanza -> m InResponse

data RequestIQResponse
  = IQResult [Element]
  | IQError StanzaError
  | -- Needed sometimes to prevent information leaks, for example https://xmpp.org/rfcs/rfc6121.html#roster-syntax-actions-push
    IQSilent
  deriving (Show, Eq)

type IQHandler m = InRequestIQ -> m RequestIQResponse

{- | Parse a 'StanzaError' out of a stanza's children. Fails with a
@'Left' StanzaError@ describing the parse failure if the @\<error/\>@
child is missing, if its @type@ attribute is missing/invalid, or if no
recognised condition element is present.
-}
parseStanzaError :: [Element] -> Either StanzaError StanzaError
parseStanzaError children = do
  errorE <- case filter ((== jcName "error") . elementName) children of
    (err : _) -> Right err
    _ -> Left $ badRequest "missing <error/> child"
  szeType <- case getAttr "type" errorE of
    Nothing -> Left $ badRequest "missing error type attribute"
    Just t -> case injFrom t of
      Just et -> Right et
      Nothing -> Left $ badRequest [i|invalid error type #{t}|]
  let cur = fromElement errorE
  szeCondition <- case listToMaybe (cur $/ curAnyElement) of
    Nothing -> Left $ badRequest "missing error condition element"
    Just en -> case injFrom (nameLocalName (elementName en)) of
      Just c -> Right c
      Nothing -> Left $ badRequest [i|unknown error condition #{nameLocalName (elementName en)}|]
  let szeText = listToMaybe $ cur $/ XC.element (stanzaName "text") &/ content
      szeChildren = cur $/ checkName (\n -> n /= stanzaName (injTo szeCondition) && n /= stanzaName "text") &| curElement
  return StanzaError {..}

checkOrFail :: (Monad m) => Maybe a -> m () -> MaybeT m a
checkOrFail Nothing finalize = MaybeT $ finalize >> return Nothing
checkOrFail (Just a) _ = MaybeT $ return $ Just a

{- | Parse a top-level @\<message\>@ or @\<presence\>@ 'Element' into an
'InStanza'. Fails for @\<iq\>@ and unknown top-level names. Error-typed
stanzas are parsed successfully — call 'getStanzaError' on 'istChildren' at
the destination to extract the 'StanzaError' when it matters.
-}
parseInStanza :: Element -> Either StanzaError InStanza
parseInStanza e = do
  istFrom <- parseAddr "from"
  istTo <- parseAddr "to"
  istType <-
    if
      | ename == jcName "message" -> case tmtype of
          Nothing -> Right $ InMessage MessageNormal
          Just t -> case injFrom t of
            Just mt -> Right $ InMessage mt
            Nothing -> Left $ badRequest "invalid message type"
      | ename == jcName "presence" -> case tmtype of
          Nothing -> Right $ InPresence Nothing
          Just t -> case injFrom t of
            Just pt -> Right $ InPresence (Just pt)
            Nothing -> Left $ badRequest "invalid presence type"
      | otherwise -> Left $ badRequest "unknown stanza type"
  return InStanza {istFrom, istTo, istId = tmid, istType, istChildren = payload}
 where
  ename = elementName e
  payload = mapMaybe (\case NodeElement ne -> Just ne; _ -> Nothing) $ elementNodes e
  tmtype = getAttr "type" e
  tmid = getAttr "id" e
  parseAddr name = case getAttr name e of
    Nothing -> Right Nothing
    Just addr -> case xmppAddress addr of
      Left _ -> Left $ jidMalformed [i|malformed address #{addr}|]
      Right a -> Right (Just a)

data SessionHooks m = SessionHooks
  { hookInHandler :: InHandler m
  , hookIQHandler :: IQHandler m
  , hookOnReconnect :: m ()
  }

stanzaSessionStep :: (MonadStream m) => StanzaSession m -> SessionHooks m -> m ()
stanzaSessionStep sess@StanzaSession {..} (SessionHooks {..}) = void $ runMaybeT $ do
  step <- MaybeT $ sessionStep ssSession
  case step of
    SessionReconnected -> lift hookOnReconnect
    SessionStanza e -> handleStanza e
 where
  handleStanza e = do
    let inHandler = hookInHandler
        reqHandler = hookIQHandler
        sendError = stanzaSendError sess e
        sendErrorOnIq err
          | ename == jcName "iq" = sendError err
          | otherwise = $(logWarn) [i|Error in received non-IQ stanza: #{err}|]
        getAddr name = mapM extractAddr $ getAttr name e
         where
          extractAddr addr = checkOrFail (toRight $ xmppAddress addr) $ sendErrorOnIq $ jidMalformed [i|malformed address #{addr}|]
        ename = elementName e
        payload = mapMaybe (\case NodeElement ne -> Just ne; _ -> Nothing) $ elementNodes e
        tmtype = getAttr "type" e
        tmid = getAttr "id" e
    tfrom <- getAddr "from"
    tto <- getAddr "to"

    if ename == jcName "iq"
      then do
        ttype <- checkOrFail tmtype $ sendError $ badRequest "iq type is not specified"
        tid <- checkOrFail tmid $ sendError $ badRequest "iq id is not specified"
        case injFrom ttype of
          Just reqType -> lift $ do
            res <-
              reqHandler
                InRequestIQ
                  { iriFrom = tfrom
                  , iriTo = tto
                  , iriId = tid
                  , iriType = reqType
                  , iriChildren = payload
                  }
            case res of
              IQSilent -> return ()
              IQError serr -> sendError serr
              IQResult cres -> do
                let attrs =
                      [ ("id", tid)
                      , ("type", "result")
                      ]
                        ++ catMaybes
                          [ ("to",) <$> getAttr "from" e
                          , ("from",) <$> getAttr "to" e
                          ]
                sessionSend ssSession $ element (jcName "iq") attrs $ map NodeElement cres
          Nothing -> case injFrom ttype of
            Just resType -> lift $ do
              let tfrom' = normalizeIQEntity ssSession tfrom
              mhandler <- atomicModifyIORef' ssRequests $ \requests -> case M.lookup (tfrom', tid) requests of
                Nothing -> (requests, Nothing)
                Just handler -> (M.delete (tfrom', tid) requests, Just handler)
              case (mhandler, resType) of
                (Nothing, _) -> sendError $ badRequest "corresponding request for response is not found"
                (Just handler, IQTypeResult) -> handler $ Right payload
                (Just handler, IQTypeError) -> handler $ Left $ either id id $ parseStanzaError payload
            Nothing -> lift $ sendError $ badRequest "iq type is invalid"
      else do
        st <- case parseInStanza e of
          Left err -> do
            lift $ sendErrorOnIq err
            mzero
          Right s -> return s
        res <- lift $ inHandler st
        case res of
          InSilent -> return ()
          InError err -> lift $ sendError err

stanzaSessionCreate :: (MonadStream m) => Session m -> m (StanzaSession m)
stanzaSessionCreate ssSession = do
  ssRequests <- newIORef M.empty
  ssNextIqId <- newIORef 0
  return StanzaSession {..}
