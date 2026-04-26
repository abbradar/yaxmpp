{-# LANGUAGE Strict #-}

-- | XEP-0115: Entity Capabilities and XEP-0390: Entity Capabilities 2.0
module Network.XMPP.XEP.Capabilities (
  HashAlgo,
  HashValue,

  -- * XEP-0115
  CapsInstance (..),
  CapsInfo (..),

  -- * XEP-0390
  Caps2Info (..),

  -- * Plugin
  capsPlugin,
) where

import Control.Codec (Codec (..))
import qualified Control.Codec as Codec
import Control.Monad.Logger
import Data.List (partition)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.String.Interpolate (i)
import Data.Text (Text)
import Text.XML
import Text.XML.Cursor hiding (element)
import qualified Text.XML.Cursor as XC

import qualified Data.Registry as Reg
import Network.XMPP.Address (FullJID)
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Stream
import Network.XMPP.XML

-- | Hash algorithm name (e.g. "sha-1", "sha-256").
type HashAlgo = Text

-- | Base64-encoded hash value.
type HashValue = Text

-- * XEP-0115: Entity Capabilities

_capsNS :: Text
capsName :: Text -> Name
(_capsNS, capsName) = namePair "http://jabber.org/protocol/caps"

data CapsInstance = CapsInstance
  { capsHash :: HashAlgo
  , capsNode :: Text
  , capsVer :: HashValue
  }
  deriving (Show, Eq)

{- | XEP-0115 capabilities parsed from presence. May contain multiple entries
from multiple @\<c\>@ elements with different hash algorithms or nodes.
-}
newtype CapsInfo = CapsInfo
  { capsInstances :: [CapsInstance]
  }
  deriving (Show, Eq)

parseCapsInstance :: Element -> Either String CapsInstance
parseCapsInstance e = do
  capsHash <- maybe (Left "missing hash attribute on <c>") Right $ getAttr "hash" e
  capsNode <- maybe (Left "missing node attribute on <c>") Right $ getAttr "node" e
  capsVer <- maybe (Left "missing ver attribute on <c>") Right $ getAttr "ver" e
  return CapsInstance {..}

capsInstanceToElement :: CapsInstance -> Element
capsInstanceToElement (CapsInstance {..}) =
  Element
    { elementName = capsName "c"
    , elementAttributes =
        M.fromList
          [ ("hash", capsHash)
          , ("node", capsNode)
          , ("ver", capsVer)
          ]
    , elementNodes = []
    }

-- * XEP-0390: Entity Capabilities 2.0

_caps2NS :: Text
caps2Name :: Text -> Name
(_caps2NS, caps2Name) = namePair "urn:xmpp:caps"

_hashesNS :: Text
hashesName :: Text -> Name
(_hashesNS, hashesName) = namePair "urn:xmpp:hashes:2"

-- | XEP-0390 capabilities: a map from hash algorithm to verification hash.
newtype Caps2Info = Caps2Info
  { caps2Hashes :: M.Map HashAlgo HashValue
  }
  deriving (Show, Eq)

parseCaps2Info :: Element -> Either String Caps2Info
parseCaps2Info e =
  let cur = fromElement e
      hashes = M.fromList $ mapMaybe parseHash $ cur $/ XC.element (hashesName "hash") &| curElement
   in if M.null hashes then Left "no hashes in <c xmlns=\"urn:xmpp:caps\">" else Right Caps2Info {caps2Hashes = hashes}
 where
  parseHash h = do
    algo <- getAttr "algo" h
    let val = mconcat $ fromElement h $/ content
    if val == "" then Nothing else Just (algo, val)

caps2InfoToElement :: Caps2Info -> Element
caps2InfoToElement (Caps2Info {..}) =
  element (caps2Name "c") [] $
    map hashToNode (M.toList caps2Hashes)
 where
  hashToNode (algo, val) =
    NodeElement $
      element (hashesName "hash") [("algo", algo)] [NodeContent val]

-- * Plugin

data CapsPlugin = CapsPlugin

instance (MonadStream m) => Codec m FullJID Presence CapsPlugin where
  codecDecode _ _ pres = case extractCaps (presenceRaw pres) of
    Left err -> do
      $(logError) [i|XEP-0115/0390 entity capabilities: #{err}|]
      return pres
    Right (mcaps1, mcaps2, raw') ->
      let ext = presenceExtended pres
          ext' = maybe ext (\c -> Reg.insert c ext) mcaps1
          ext'' = maybe ext' (\c -> Reg.insert c ext') mcaps2
       in return $ pres {presenceRaw = raw', presenceExtended = ext''}

  codecEncode _ _ pres =
    let ext = presenceExtended pres
        (mcaps1, ext') = Reg.pop (Proxy :: Proxy CapsInfo) ext
        (mcaps2, ext'') = Reg.pop (Proxy :: Proxy Caps2Info) ext'
        raw = presenceRaw pres
        raw' = maybe raw (\c -> map capsInstanceToElement (capsInstances c) ++ raw) mcaps1
        raw'' = maybe raw' (\c -> caps2InfoToElement c : raw') mcaps2
     in return $ pres {presenceRaw = raw'', presenceExtended = ext''}

extractCaps :: [Element] -> Either String (Maybe CapsInfo, Maybe Caps2Info, [Element])
extractCaps elems = do
  let (caps1Elems, rest1) = partition (\e -> elementName e == capsName "c") elems
      (caps2Elems, rest2) = partition (\e -> elementName e == caps2Name "c") rest1
  caps1Items <- traverse parseCapsInstance caps1Elems
  let mcaps1 = case caps1Items of
        [] -> Nothing
        xs -> Just $ CapsInfo xs
  mcaps2 <- case caps2Elems of
    [] -> Right Nothing
    (e : _) -> Just <$> parseCaps2Info e
  return (mcaps1, mcaps2, rest2)

capsPlugin :: forall m. (MonadStream m) => XMPPPluginsRef m -> m ()
capsPlugin pluginsRef = do
  pp <- getPresencePlugin pluginsRef
  Codec.pushNewOrFailM CapsPlugin (presencePluginCodecs pp)
