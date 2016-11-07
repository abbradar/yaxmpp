module Network.XMPP.Address.Internal where

import Data.Monoid
import Data.Text (Text)
import Data.Aeson
import Control.Applicative
import Control.Monad.Fail (MonadFail)
import Data.Text (Text)
import Data.Attoparsec.Text
import Text.StringPrep
import Text.StringPrep.Profiles

data XMPPAddress = XMPPAddress { xmppLocal :: Maybe Text
                               , xmppDomain :: Text
                               , xmppResource :: Maybe Text
                               }
                 deriving (Eq, Ord)

instance Show XMPPAddress where
  show = show . showXMPPAddress

instance FromJSON XMPPAddress where
  parseJSON = withText "XMPPAddress" $ \t -> case readXMPPAddress t of
    Nothing -> fail "XMPPAddress"
    Just r -> return r

instance ToJSON XMPPAddress where
  toJSON = toJSON . showXMPPAddress

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

maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail _ (Just a) = return a
maybeFail err Nothing = fail err

parseXMPPAddress :: Parser XMPPAddress
parseXMPPAddress = do
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
        _ -> error "parseXMPPAddress: impossible second separator"
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
    _ -> error "parseXMPPAddress: impossible first separator"

  where checkLocal = maybeFail "parseXMPPAddress: localpart doesn't satisfy Nodeprep profile of stringprep" . runStringPrep nodePrepProfile
        checkDomain = maybeFail "parseXMPPAddress: domainpart doesn't satisfy Nameprep profile of stringprep" . runStringPrep xmppNamePrepProfile
        checkResource = maybeFail "parseXMPPAddress: resourcepart doesn't satisfy Resourceprep profile of stringprep" . runStringPrep resourcePrepProfile

readXMPPAddress :: Text -> Maybe XMPPAddress
readXMPPAddress addr = case parseOnly (parseXMPPAddress <* endOfInput) addr of
  Left _ -> Nothing
  Right res -> Just res

xmppAddress :: Maybe Text -> Text -> Maybe Text -> Maybe XMPPAddress
xmppAddress local domain resource = do
  xmppLocal <- mapM (runStringPrep nodePrepProfile) local
  xmppDomain <- runStringPrep xmppNamePrepProfile domain
  xmppResource <- mapM (runStringPrep resourcePrepProfile) resource
  return XMPPAddress {..}

showXMPPAddress :: XMPPAddress -> Text
showXMPPAddress (XMPPAddress {..}) = maybe mempty (<> "@") xmppLocal <> xmppDomain <> maybe mempty ("/" <>) xmppResource
