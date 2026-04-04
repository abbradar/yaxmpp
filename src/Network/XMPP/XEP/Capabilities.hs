{-# LANGUAGE Strict #-}

module Network.XMPP.XEP.Capabilities (
  CapsInfo (..),
  parseCaps,
) where

import Control.Monad
import Data.Text (Text)
import Text.XML

import Network.XMPP.XML

_capsNS :: Text
capsName :: Text -> Name
(_capsNS, capsName) = namePair "http://jabber.org/protocol/caps"

data CapsInfo = CapsInfo
  { capsHash :: Text
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
