{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}

-- | XEP-0313: Message Archive Management (version 2, @urn:xmpp:mam:2@).
module Network.XMPP.XEP.MAM (
  MAMFilter (..),
  MAMMessage (..),
  MAMInput (..),
  MAMOutput (..),
  MAMHandler (..),
  MAMPage (..),
  MAMArchiveBound (..),
  MAMMetadata (..),
  MAMPlugin,
  getMAMPlugin,
  mamQuery,
  mamMetadata,
  mamPlugin,
) where

import Control.Applicative ((<|>))
import Control.HandlerList (Handler (..))
import qualified Control.HandlerList as HL
import Control.MemoAsync (MemoAsync)
import qualified Control.MemoAsync as MemoAsync
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Registry.Mutable as RegRef
import qualified Data.Set as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import UnliftIO.IORef

import Data.Time.XMPP
import Network.XMPP.Address
import Network.XMPP.Plugin
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.DelayedDelivery
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.Forms
import Network.XMPP.XEP.Forwarding
import Network.XMPP.XEP.RSM
import Network.XMPP.XML

mamNS :: Text
mamName :: Text -> Name
(mamNS, mamName) = namePair "urn:xmpp:mam:2"

{- | Feature string for the @#extended@ subset (XEP-0313 §4.1.5, §5): before-id /
after-id filters, flipped pages, and archive metadata.
-}
mamExtendedNS :: Text
mamExtendedNS = mamNS <> "#extended"

-- | Feature string for @include-groupchat@ field support (XEP-0313 §4.4.6).
mamGroupchatFieldNS :: Text
mamGroupchatFieldNS = mamNS <> "#groupchat-field"

{- | Filter parameters for a MAM query. The 'Semigroup' instance is left-biased
(first 'Just' on each field wins); 'mempty' is an all-'Nothing' filter.
-}
data MAMFilter = MAMFilter
  { mamFilterWith :: Maybe XMPPAddress
  , mamFilterStart :: Maybe UTCTime
  , mamFilterEnd :: Maybe UTCTime
  , mamFilterBeforeId :: Maybe Text
  -- ^ Archive id. Requires the @#extended@ feature.
  , mamFilterAfterId :: Maybe Text
  -- ^ Archive id. Requires the @#extended@ feature.
  , mamFilterIncludeGroupchat :: Maybe Bool
  {- ^ Whether to include group chat messages in the results (XEP-0313 §4.4.6).
  'Nothing' omits the field and leaves the server default in place.
  -}
  }
  deriving (Show, Eq)

instance Semigroup MAMFilter where
  a <> b =
    MAMFilter
      { mamFilterWith = mamFilterWith a <|> mamFilterWith b
      , mamFilterStart = mamFilterStart a <|> mamFilterStart b
      , mamFilterEnd = mamFilterEnd a <|> mamFilterEnd b
      , mamFilterBeforeId = mamFilterBeforeId a <|> mamFilterBeforeId b
      , mamFilterAfterId = mamFilterAfterId a <|> mamFilterAfterId b
      , mamFilterIncludeGroupchat = mamFilterIncludeGroupchat a <|> mamFilterIncludeGroupchat b
      }

instance Monoid MAMFilter where
  mempty = MAMFilter Nothing Nothing Nothing Nothing Nothing Nothing

-- | Features a filter needs from the archive to run.
filterFeatures :: MAMFilter -> MAMFeatures
filterFeatures MAMFilter {..} =
  mempty
    { mamFeaturesExtended = isJust mamFilterBeforeId || isJust mamFilterAfterId
    , mamFeaturesGroupchatField = isJust mamFilterIncludeGroupchat
    }

{- | Feature flags detected for the archive on the bare-JID. 'Semigroup' ORs
each field; 'mempty' is all-'False'. When used to express /required/ features,
combining with '<>' yields the union of all requirements.
-}
data MAMFeatures = MAMFeatures
  { mamFeaturesExtended :: Bool
  -- ^ @#extended@ subset: before-id/after-id, flipped pages, metadata query.
  , mamFeaturesGroupchatField :: Bool
  -- ^ @#groupchat-field@: @include-groupchat@ form field support.
  }
  deriving (Show, Eq)

instance Semigroup MAMFeatures where
  a <> b =
    MAMFeatures
      { mamFeaturesExtended = mamFeaturesExtended a || mamFeaturesExtended b
      , mamFeaturesGroupchatField = mamFeaturesGroupchatField a || mamFeaturesGroupchatField b
      }

instance Monoid MAMFeatures where
  mempty = MAMFeatures False False

{- | Check that every feature required by the first argument is advertised by
the second. Returns the first missing feature as a @feature-not-implemented@
error.
-}
checkMAMFeatures :: MAMFeatures -> MAMFeatures -> Either StanzaError ()
checkMAMFeatures required available
  | mamFeaturesExtended required && not (mamFeaturesExtended available) =
      missing mamExtendedNS
  | mamFeaturesGroupchatField required && not (mamFeaturesGroupchatField available) =
      missing mamGroupchatFieldNS
  | otherwise = Right ()
 where
  missing ns = Left $ featureNotImplemented [i|#{ns} not supported by the archive|]

-- | A single archived entry delivered within a page.
data MAMMessage = MAMMessage
  { mamMsgArchiveId :: Text
  , mamMsgTimestamp :: ZonedTime
  {- ^ Server-storage timestamp from the mandatory @\<delay/\>@ element
  (XEP-0313 §5.1.2).
  -}
  , mamMsgElement :: Element
  -- ^ The archived @\<message\>@ stanza verbatim. Consumers decide how to
  -- interpret it (e.g. pass to 'parseIMMessage').
  }
  deriving (Show)

{- | Input to a 'MAMHandler'. 'InMsg' carries a parsed result or a per-result
parse error; 'InEnd' signals that the page has been fully delivered (or that
the IQ itself failed) and carries the final 'MAMPage' summary or error. The
@last@ tag is 'True' on the terminating call and 'False' on every streamed
result; it links the input shape to the matching 'MAMOutput' shape.
-}
data MAMInput (last :: Bool) where
  InMsg :: Either StanzaError MAMMessage -> MAMInput 'False
  InEnd :: Either StanzaError MAMPage -> MAMInput 'True

{- | Reply from a 'MAMHandler'. 'OutNext' returns the continuation handler for
the next input; 'OutDone' acknowledges end-of-stream. The @last@ tag matches
the 'MAMInput' tag — an 'InMsg' must be answered with 'OutNext', 'InEnd' with
'OutDone', enforced statically.
-}
data MAMOutput (last :: Bool) m where
  OutNext :: MAMHandler m -> MAMOutput 'False m
  OutDone :: MAMOutput 'True m

{- | A fold-style handler for one MAM query. Each result is fed as
@'InMsg' x@, yielding the next handler; the terminating call @'InEnd'@
yields 'OutDone'.
-}
newtype MAMHandler m = MAMHandler
  { runMAMHandler :: forall (last :: Bool). MAMInput last -> m (MAMOutput last m)
  }

{- | Summary of a delivered page. Messages are streamed to the 'MAMHandler';
this carries only the @\<fin\>@ info.
-}
data MAMPage = MAMPage
  { mamPageComplete :: Bool
  , mamPageSet :: Maybe ResultSet
  }
  deriving (Show)

-- | Boundary of the archive (start or end), reported by a metadata query.
data MAMArchiveBound = MAMArchiveBound
  { mamBoundId :: Text
  , mamBoundTimestamp :: UTCTime
  }
  deriving (Show, Eq)

-- | Archive metadata (XEP-0313 §5); requires the @#extended@ feature.
data MAMMetadata = MAMMetadata
  { mamMetaStart :: Maybe MAMArchiveBound
  , mamMetaEnd :: Maybe MAMArchiveBound
  }
  deriving (Show, Eq)

data MAMPlugin m = MAMPlugin
  { mamPluginSession :: StanzaSession m
  , mamPluginDisco :: MemoAsync m (Maybe MAMFeatures)
  , mamPluginQueries :: IORef (Map Text (IORef (MAMHandler m)))
  , mamPluginNextQueryId :: IORef Word
  }

mamForm :: MAMFilter -> Element
mamForm MAMFilter {..} =
  submitForm $
    formTypeField mamNS
      : catMaybes
        [ fmap (\a -> formField "with" Nothing (addressToText a)) mamFilterWith
        , fmap (\t -> formField "start" Nothing (utcTimeToXmpp t)) mamFilterStart
        , fmap (\t -> formField "end" Nothing (utcTimeToXmpp t)) mamFilterEnd
        , fmap (formField "before-id" Nothing) mamFilterBeforeId
        , fmap (formField "after-id" Nothing) mamFilterAfterId
        , fmap (\b -> formField "include-groupchat" (Just "boolean") (if b then "true" else "false")) mamFilterIncludeGroupchat
        ]

parseFin :: Element -> Either StanzaError (Bool, Maybe ResultSet)
parseFin e
  | elementName e == mamName "fin" = do
      let complete = getAttr "complete" e == Just "true"
      rs <- case parseRSM e of
        Left err -> Left $ badRequest $ T.pack err
        Right r -> Right r
      Right (complete, rs)
  | otherwise = Left $ badRequest "expected <fin>"

parseMetadata :: Element -> Either StanzaError MAMMetadata
parseMetadata e
  | elementName e == mamName "metadata" = do
      let cur = fromElement e
      mamMetaStart <- parseBound (mamName "start") cur
      mamMetaEnd <- parseBound (mamName "end") cur
      Right MAMMetadata {..}
  | otherwise = Left $ badRequest "expected <metadata>"
 where
  parseBound n c = case c $/ XC.element n &| curElement of
    [] -> Right Nothing
    (b : _) -> case (getAttr "id" b, getAttr "timestamp" b) of
      (Just bid, Just ts) -> case xmppZonedTime ts of
        Left err -> Left $ badRequest $ T.pack err
        Right zt -> Right $ Just MAMArchiveBound {mamBoundId = bid, mamBoundTimestamp = zonedTimeToUTC zt}
      _ -> Left $ badRequest [i|incomplete #{nameLocalName n} archive bound|]

extractMAMResult :: [Element] -> Maybe (Text, Text, Element)
extractMAMResult = listToMaybe . mapMaybe tryOne
 where
  tryOne e
    | elementName e == mamName "result" = do
        qid <- getAttr "queryid" e
        mid <- getAttr "id" e
        fwdEl <- listToMaybe $ fromElement e $/ XC.element (nsName forwardNS "forwarded") &| curElement
        return (qid, mid, fwdEl)
    | otherwise = Nothing

{- | Parse an archived @\<forwarded\>@ result into a 'MAMMessage'. Extracts the
mandatory @\<delay/\>@ timestamp and hands the inner @\<message\>@ stanza to
the caller verbatim, leaving IM-level parsing to the consumer.
-}
parseMAMResult :: Text -> Element -> Either StanzaError MAMMessage
parseMAMResult mid fwdEl = case parseForwarded fwdEl of
  Left err -> Left err
  Right Nothing -> Left $ badRequest "expected <forwarded>"
  Right (Just Forwarded {fwdDelay = Nothing}) ->
    Left $ badRequest "missing <delay/> (XEP-0313 §5.1.2)"
  Right (Just Forwarded {fwdDelay = Just delay, fwdMessage}) ->
    Right $ MAMMessage mid (delayStamp delay) fwdMessage

instance (MonadStream m) => Handler m InStanza InResponse (MAMPlugin m) where
  tryHandle (MAMPlugin {mamPluginQueries}) (InStanza {istType = InMessage (Right _), istChildren})
    | Just (qid, mid, fwdEl) <- extractMAMResult istChildren = do
        queries <- readIORef mamPluginQueries
        case M.lookup qid queries of
          Nothing -> return $ Just InSilent
          Just handlerRef -> do
            MAMHandler handler <- readIORef handlerRef
            OutNext nextHandler <- handler (InMsg (parseMAMResult mid fwdEl))
            writeIORef handlerRef nextHandler
            return $ Just InSilent
  tryHandle _ _ = return Nothing

getMAMPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m (MAMPlugin m)
getMAMPlugin pluginsRef = RegRef.lookupOrFailM (Proxy :: Proxy (MAMPlugin m)) $ pluginsHooksSet pluginsRef

{- | Resolve MAM support. Calls the handler with @Left@ if MAM itself is
unsupported or any feature required by @needed@ is missing, and @Right ()@
once support is confirmed.
-}
requireMam :: (MonadStream m) => MAMPlugin m -> MAMFeatures -> (Either StanzaError () -> m ()) -> m ()
requireMam (MAMPlugin {mamPluginDisco}) needed handler =
  MemoAsync.get mamPluginDisco $ \case
    Nothing -> handler $ Left $ featureNotImplemented [i|#{mamNS} not supported by the archive|]
    Just feats -> handler $ checkMAMFeatures needed feats

{- | Execute a MAM query. Verifies disco support (@urn:xmpp:mam:2@, plus
@#extended@ if the filter uses before-id/after-id) and fails with
@feature-not-implemented@ otherwise. Each archived result is streamed into
the supplied 'MAMHandler' as @'InMsg' ...@; after the @\<fin\>@ reply (or on
an IQ error) the handler receives one final @'InEnd' (Either StanzaError
MAMPage)@ carrying the page summary. Walk further pages by reissuing with an
updated 'SetQuery' (typically @'After' (rsmLast ...)@ taken from
'mamPageSet').
-}
mamQuery :: (MonadStream m) => MAMPlugin m -> MAMFilter -> SetQuery -> MAMHandler m -> m ()
mamQuery plug@(MAMPlugin {..}) filt setq initialHandler = requireMam plug (filterFeatures filt) $ \case
  Left e -> do
    MAMHandler handler <- return initialHandler
    OutDone <- handler (InEnd (Left e))
    return ()
  Right () -> do
    qid <- atomicModifyIORef' mamPluginNextQueryId $ \n -> (n + 1, T.pack (show n))
    handlerRef <- newIORef initialHandler
    atomicModifyIORef' mamPluginQueries $ \m -> (M.insert qid handlerRef m, ())
    let queryEl =
          element
            (mamName "query")
            [("queryid", qid)]
            [NodeElement $ mamForm filt, NodeElement $ rsmElement setq]
    stanzaRequest mamPluginSession (serverRequest IQSet [queryEl]) $ \resp -> do
      atomicModifyIORef' mamPluginQueries $ \m -> (M.delete qid m, ())
      let finalPayload = case resp of
            Left e -> Left e
            Right [finE] -> case parseFin finE of
              Left e -> Left e
              Right (complete, rs) -> Right $ MAMPage complete rs
            Right _ -> Left $ badRequest "invalid MAM response"
      MAMHandler handler <- readIORef handlerRef
      OutDone <- handler (InEnd finalPayload)
      return ()

-- | Archive metadata query (XEP-0313 §5). Requires the @#extended@ feature.
mamMetadata :: (MonadStream m) => MAMPlugin m -> (Either StanzaError MAMMetadata -> m ()) -> m ()
mamMetadata plug@(MAMPlugin {mamPluginSession}) handler = requireMam plug needed $ \case
  Left e -> handler $ Left e
  Right () ->
    stanzaRequest mamPluginSession (serverRequest IQGet [closedElement (mamName "metadata")]) $ \case
      Left e -> handler $ Left e
      Right [metaE] ->
        case parseMetadata metaE of
          Left e -> handler $ Left e
          Right m -> handler $ Right m
      Right _ -> handler $ Left $ badRequest "invalid MAM metadata response"
 where
  needed = mempty {mamFeaturesExtended = True}

mamPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
mamPlugin pluginsRef = do
  let mamPluginSession = pluginsSession pluginsRef
  dp <- getDiscoPlugin pluginsRef
  mamPluginDisco <- MemoAsync.new $ \cb ->
    getSelfDiscoEntity dp $ \case
      Left _ -> cb Nothing
      Right ent ->
        let feats = discoFeatures ent
         in cb $
              if mamNS `S.member` feats
                then
                  Just
                    MAMFeatures
                      { mamFeaturesExtended = mamExtendedNS `S.member` feats
                      , mamFeaturesGroupchatField = mamGroupchatFieldNS `S.member` feats
                      }
                else Nothing
  mamPluginQueries <- newIORef M.empty
  mamPluginNextQueryId <- newIORef 0
  let plugin :: MAMPlugin m = MAMPlugin {..}
  RegRef.insertNewOrFailM plugin $ pluginsHooksSet pluginsRef
  HL.pushNewOrFailM plugin $ pluginsInHandlers pluginsRef
