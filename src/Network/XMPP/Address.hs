module Network.XMPP.Address
       ( XMPPLocal
       , localText
       , localFromText
       , XMPPDomain
       , domainText
       , domainFromText
       , XMPPResource
       , resourceText
       , resourceFromText
       , XMPPAddress(..)
       , xmppAddress'
       , xmppAddress
       , addressToText
       , addressBare
       , BareJID(..)
       , bareJidGet
       , bareJidAddress
       , bareJidToText
       , FullJID(..)
       , fullJidGet
       , fullJidAddress
       , fullJidToText
       ) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Control.Applicative
import Data.Attoparsec.Text
import Text.StringPrep
import Text.StringPrep.Profiles

import Network.XMPP.Utils

newtype XMPPLocal = XMPPLocal { localText :: Text }
                  deriving (Eq, Ord, Show)

localFromText :: Text -> Maybe XMPPLocal
localFromText t = XMPPLocal <$> runStringPrep nodePrepProfile t

newtype XMPPDomain = XMPPDomain { domainText :: Text }
                   deriving (Eq, Ord, Show)

instance FromJSON XMPPDomain where
  parseJSON = withText "XMPPDomain" $ \t -> case domainFromText t of
    Nothing -> fail "XMPPDomain"
    Just r -> return r

instance ToJSON XMPPDomain where
  toJSON = toJSON . domainText

domainFromText :: Text -> Maybe XMPPDomain
domainFromText t = XMPPDomain <$> runStringPrep xmppNamePrepProfile t

newtype XMPPResource = XMPPResource { resourceText :: Text }
                     deriving (Eq, Ord, Show)

resourceFromText :: Text -> Maybe XMPPResource
resourceFromText t = XMPPResource <$> runStringPrep resourcePrepProfile t

data XMPPAddress = XMPPAddress { addressLocal :: Maybe XMPPLocal
                               , addressDomain :: XMPPDomain
                               , addressResource :: Maybe XMPPResource
                               }
                 deriving (Eq, Ord)

instance Show XMPPAddress where
  show = show . addressToText

instance FromJSON XMPPAddress where
  parseJSON = withText "XMPPAddress" $ \t -> case xmppAddress t of
    Left e -> fail e
    Right r -> return r

instance FromJSONKey XMPPAddress where
  fromJSONKey = FromJSONKeyTextParser $ \k -> case xmppAddress k of
    Left e -> fail e
    Right r -> return r

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
          , prohibited = [a1, c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
          , shouldCheckBidi = True
          }

xmppAddress' :: Parser XMPPAddress
xmppAddress' = do
  first <- takeTill (\c -> c == '@' || c == '/')
  sep <- optional anyChar
  case sep of
    Just '@' -> do
      addressLocal <- Just <$> checkLocal first
      addressDomain <- takeTill (== '/') >>= checkDomain
      sep2 <- optional anyChar
      case sep2 of
        Just '/' -> do
          addressResource <- Just <$> (takeText >>= checkResource)
          return XMPPAddress { .. }
        Nothing -> return XMPPAddress { addressResource = Nothing
                                     , ..
                                     }
        _ -> error "xmppAddress: impossible second separator"
    Just '/' -> do
      addressDomain <- checkDomain first
      addressResource <- Just <$> (takeText >>= checkResource)
      return XMPPAddress { addressLocal = Nothing
                         , ..
                         }
    Nothing -> do
      addressDomain <- checkDomain first
      return XMPPAddress { addressLocal = Nothing
                         , addressResource = Nothing
                         , ..
                         }
    _ -> error "xmppAddress: impossible first separator"

  where checkLocal = maybeFail "xmppAddress: localpart doesn't satisfy Nodeprep profile of stringprep" . localFromText
        checkDomain = maybeFail "xmppAddress: domainpart doesn't satisfy Nameprep profile of stringprep" . domainFromText
        checkResource = maybeFail "xmppAddress: resourcepart doesn't satisfy Resourceprep profile of stringprep" . resourceFromText

xmppAddress :: Text -> Either String XMPPAddress
xmppAddress = parseValue xmppAddress'

addressToText :: XMPPAddress -> Text
addressToText (XMPPAddress {..}) =
  maybe mempty ((<> "@") . localText) addressLocal
  <> domainText addressDomain
  <> maybe mempty (("/" <>) . resourceText) addressResource

addressBare :: XMPPAddress -> XMPPAddress
addressBare (XMPPAddress {..}) = XMPPAddress { addressResource = Nothing
                                             , ..
                                             }

data BareJID = BareJID { bareLocal :: XMPPLocal
                       , bareDomain :: XMPPDomain
                       }
             deriving (Eq, Ord)

bareJidAddress :: BareJID -> XMPPAddress
bareJidAddress (BareJID {..}) = XMPPAddress (Just bareLocal) bareDomain Nothing

bareJidToText :: BareJID -> Text
bareJidToText = addressToText . bareJidAddress

instance Show BareJID where
  show = show . bareJidToText

instance FromJSON BareJID where
  parseJSON v = do
    addr <- parseJSON v
    case bareJidGet addr of
      Nothing -> fail "BareJID"
      Just r -> return r

instance ToJSON BareJID where
  toJSON = toJSON . bareJidAddress

bareJidGet :: XMPPAddress -> Maybe BareJID
bareJidGet XMPPAddress { addressLocal = Just bareLocal, addressDomain = bareDomain, addressResource = Nothing } = Just BareJID {..}
bareJidGet _ = Nothing

data FullJID = FullJID { fullBare :: BareJID
                       , fullResource :: XMPPResource
                       }
             deriving (Eq, Ord)

fullJidAddress :: FullJID -> XMPPAddress
fullJidAddress (FullJID {..}) = XMPPAddress (Just $ bareLocal fullBare) (bareDomain fullBare) (Just fullResource)

fullJidToText :: FullJID -> Text
fullJidToText = addressToText . fullJidAddress

instance Show FullJID where
  show = show . fullJidToText

instance FromJSON FullJID where
  parseJSON v = do
    addr <- parseJSON v
    case fullJidGet addr of
      Nothing -> fail "FullJID"
      Just r -> return r

instance ToJSON FullJID where
  toJSON = toJSON . fullJidAddress

fullJidGet :: XMPPAddress -> Maybe FullJID
fullJidGet XMPPAddress {..} = do
  bareLocal <- addressLocal
  fullResource <- addressResource
  return FullJID { fullBare = BareJID { bareDomain = addressDomain, .. }, .. }
