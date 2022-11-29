{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Capabilities
  ( ShowState(..)
  , PresenceHandler
  , Presence(..)
  , defaultPresence
  , presencePlugin
  , PresenceEvent(..)
  , presenceUpdate
  , presenceStanza
  ) where

import Data.Int
import Data.Maybe
import Control.Monad
import Text.Read (readMaybe)
import Text.XML
import UnliftIO.IORef
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TB
import Control.Monad.Logger
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC
import Data.String.Interpolate (i)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import TextShow (showt)
import Crypto.Hash
import Crypto.Hash.Algorithms

import Data.Injective
import Network.XMPP.XML
import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Address
import Network.XMPP.Language
import Network.XMPP.Presence
import Network.XMPP.XEP.Disco

type CapsVersion = Text
type CapsEntries = Map CapsVersion DiscoEntity

data CapsRef m = CapsRef { cref :: IORef (Map CapsVersion (Either (Async DiscoEntity) DiscoEntity))
                         , crefSession :: StanzaSession m
                         }

discoEntityString :: DiscoEntity -> BL.ByteString
discoEntityString (DiscoEntity {..}) = TB.encodeUtf8 $ TB.toLazyText $ mconcat $ concatMap (\part -> [part, TB.singleton '<']) parts
  where parts = identityStrings (M.toAscList discoIdentities) ++ map TB.fromText (S.toAscList discoFeatures)
        identityStrings (DiscoIdentity {..}) maybeLang =
          case maybeLang of
            Nothing -> [identityPrefix <> "/"]
            Just names -> mconcat $ map ((identityPrefix <>) . toLangText) $ M.toAscList $ langTexts names
          where identityPrefix = TB.fromText discoCategory <> "/" <> TB.fromText discoType <> "/"
                toLangText (mlangText, nameText) = TB.fromText (fromMaybe "" mlangText) <> "/" <> TB.fromText nameText

type HashName = Text

hashAlgorithms :: Map HashName (BL.ByteString -> (forall a. HashAlgorithm a => a))
hashAlgorithms = M.fromList [ ("sha-1", SHA1)
                            ]

defaultHashName :: HashName
defaultHashName = "sha-1"

defaultHash :: forall a. HashAlgorithm a => a
defaultHash = hashAlgorithms M.! defaultHashName

capsUserNS :: Text
capsUserName :: Text -> Name
(capsUserNS, capsUserName) = namePair "http://jabber.org/protocol/caps"

data CapsInfo = CapsInfo { capsHash :: HashName
                         , capsNode :: Text
                         , capsVer :: Text
                         }
                     deriving (Show, Eq)

parseCaps :: Element -> Maybe CapsInfo
parseCaps e = do
  guard $ elementName e == capsName "c"
  capsHash <- getAttr (capsName "hash") e
  capsNode <- getAttr (capsName "node") e
  capsVer <- getAttr (capsName "ver") e
  return $ CapsInfo {..}

getCapsCache :: MonadStream m => CapsRef m -> m CapsEntries
getCapsCache = readIORef . cref

getOrRequestPresenceCaps :: MonadStream m => CapsRef m -> XMPPAddress -> Presence -> m (Maybe DiscoEntity)
getOrRequestPresenceCaps (CapsRef {..}) address presence
  | (CapsInfo {..}):_ <- mapMaybe parseCaps (presenceExtended presence) = do
      cache <- readIORef cref
      case M.lookup capsVer cache of
        Just ent -> return (Just ent)
        Nothing ->
  | otherwise = return Nothing

presenceInHandler :: MonadStream m => PresenceRef m -> PluginInHandler m
presenceInHandler pref (InStanza { istType = InPresence (Right (presenceOp -> Just op)), istFrom = Just (fullJidGet -> Just faddr), istChildren }) = Just <$> do
  case op of
    PresenceSet -> case parsePresence istChildren of
      Right p -> do
        emitPresence pref faddr $ Right p
        return Nothing
      Left e -> return $ Just e
    PresenceUnset -> do
        emitPresence pref faddr $ Left $ parseExtended istChildren
        return Nothing
presenceInHandler _ _ = return Nothing

presencePlugin :: MonadStream m => [PresenceHandler m] -> m (XMPPPlugin m)
presencePlugin presenceHandlers = do
  let pref = PresenceRef {..}
      plugin = emptyPlugin { pluginInHandler = presenceInHandler pref }
  return plugin

data PresenceEvent k = Added k Presence
                     | Updated k Presence
                     | Removed k [Element]
                     deriving (Show, Eq)

presenceUpdate :: Ord k => k -> Either [Element] Presence -> Map k Presence -> Maybe (Map k Presence, PresenceEvent k)
presenceUpdate k (Right v) m
  | otherwise = Just (M.insert k v m, Added k v)
  | M.member k m = Just (M.insert k v m, Updated k v)
presenceUpdate k (Left e) m
  | M.member k m = Just (M.delete k m, Removed k e)
  | otherwise = Nothing

presenceStanza :: Maybe Presence -> OutStanza
presenceStanza (Just (Presence {..})) =
  OutStanza { ostTo = Nothing
            , ostType = OutPresence Nothing
            , ostChildren = [priority] ++ maybeToList mShow ++ statuses ++ presenceExtended
            }
  where priority = element (jcName "priority") [] [NodeContent $ showt presencePriority]
        mShow = fmap (\s -> element (jcName "show") [] [NodeContent $ injTo s]) presenceShow
        statuses = maybe [] (localizedElements $ jcName "status") presenceStatus
presenceStanza Nothing =
  OutStanza { ostTo = Nothing
            , ostType = OutPresence (Just PresenceUnavailable)
            , ostChildren = []
            }
