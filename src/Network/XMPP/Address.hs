module Network.XMPP.Address
       ( XMPPAddress
       , xmppLocal
       , xmppDomain
       , xmppResource
       , xmppAddress
       , addressFromText
       , addressToText
       ) where

import Data.Monoid
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Control.Applicative
import Data.Attoparsec.Text
import Text.StringPrep
import Text.StringPrep.Profiles

import Network.XMPP.Utils

data XMPPAddress = XMPPAddress { xmppLocal :: Maybe Text
                               , xmppDomain :: Text
                               , xmppResource :: Maybe Text
                               }
                 deriving (Eq, Ord)

instance Show XMPPAddress where
  show = show . addressToText

instance FromJSON XMPPAddress where
  parseJSON = withText "XMPPAddress" $ \t -> case parseValue xmppAddress t of
    Nothing -> fail "XMPPAddress"
    Just r -> return r

instance FromJSONKey XMPPAddress where
  fromJSONKey = FromJSONKeyTextParser $ \k -> case parseValue xmppAddress k of
    Nothing -> fail "XMPPAddress"
    Just r -> return r

instance ToJSON XMPPAddress where
  toJSON = toJSON . addressToText

instance ToJSONKey XMPPAddress where
  toJSONKey = toJSONKeyText addressToText

nodeProhibited :: [Range]
nodeProhibited = 
  [ range '\x0022' '\x0023'
  , range '\x0026' '\x0027'
  , range '\x0027' '\x0028'
  , range '\x002F' '\x0030'
  , range '\x003A' '\x003B'
  , range '\x003C' '\x003D'
  , range '\x003E' '\x003F'
  , range '\x0040' '\x0041'
  ]

nodePrepProfile :: StringPrepProfile
nodePrepProfile =
  Profile { maps = [b1, b2]
          , shouldNormalize = True
          , prohibited = [a1, c11, c12, c21, c22, c3, c4, c5, c6, c7, c8, c9, nodeProhibited]
          , shouldCheckBidi = True
          }

xmppNamePrepProfile :: StringPrepProfile
xmppNamePrepProfile = namePrepProfile False

resourcePrepProfile :: StringPrepProfile
resourcePrepProfile =
  Profile { maps = [b1]
          , shouldNormalize = True
          , prohibited = [a1, c11, c21, c22, c3, c4, c5, c6, c7, c8, c9]
          , shouldCheckBidi = True
          }

xmppAddress :: Parser XMPPAddress
xmppAddress = do
  first <- takeTill (\c -> c == '@' || c == '/')
  sep <- optional anyChar
  case sep of
    Just '@' -> do
      xmppLocal <- Just <$> checkLocal first
      xmppDomain <- takeTill (== '/') >>= checkDomain
      sep2 <- optional anyChar
      case sep2 of
        Just '/' -> do
          xmppResource <- Just <$> (takeText >>= checkResource)
          return XMPPAddress { .. }
        Nothing -> return XMPPAddress { xmppResource = Nothing
                                     , ..
                                     }
        _ -> error "xmppAddress: impossible second separator"
    Just '/' -> do
      xmppDomain <- checkDomain first
      xmppResource <- Just <$> (takeText >>= checkResource)
      return XMPPAddress { xmppLocal = Nothing
                         , ..
                         }
    Nothing -> do
      xmppDomain <- checkDomain first
      return XMPPAddress { xmppLocal = Nothing
                         , xmppResource = Nothing
                         , ..
                         }
    _ -> error "xmppAddress: impossible first separator"

  where checkLocal = maybeFail "xmppAddress: localpart doesn't satisfy Nodeprep profile of stringprep" . runStringPrep nodePrepProfile
        checkDomain = maybeFail "xmppAddress: domainpart doesn't satisfy Nameprep profile of stringprep" . runStringPrep xmppNamePrepProfile
        checkResource = maybeFail "xmppAddress: resourcepart doesn't satisfy Resourceprep profile of stringprep" . runStringPrep resourcePrepProfile

addressFromText :: Maybe Text -> Text -> Maybe Text -> Maybe XMPPAddress
addressFromText local domain resource = do
  xmppLocal <- mapM (runStringPrep nodePrepProfile) local
  xmppDomain <- runStringPrep xmppNamePrepProfile domain
  xmppResource <- mapM (runStringPrep resourcePrepProfile) resource
  return XMPPAddress {..}

addressToText :: XMPPAddress -> Text
addressToText (XMPPAddress {..}) = maybe mempty (<> "@") xmppLocal <> xmppDomain <> maybe mempty ("/" <>) xmppResource
