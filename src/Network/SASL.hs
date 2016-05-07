module Network.SASL
       ( ClientMessage(..)
       , ServerMessage(..)
       , SASLWire
       , runSASLWire
       , SASLAuthenticator(..)
       , plainAuth
       ) where

import Data.Monoid
import Data.ByteString (ByteString)

data ClientMessage = SASLResponse ByteString
                   | SASLAbort
                   deriving (Show, Eq)

data ServerMessage = SASLSuccess (Maybe ByteString)
                   | SASLFailure
                   | SASLChallenge ByteString
                   deriving (Show, Eq)

newtype SASLWire a = SASLWire { runSASLWire :: ServerMessage -> IO (Either (ClientMessage, SASLWire a) (Maybe a)) }

data SASLAuthenticator a = SASLAuthenticator { saslMechanism :: ByteString
                                             , saslInitial :: Maybe ByteString
                                             , saslWire :: SASLWire a
                                             }

successWire :: SASLWire ()
successWire = SASLWire $ \case
  SASLSuccess Nothing -> return $ Right $ Just ()
  SASLFailure -> return $ Right Nothing
  _ -> return $ Left (SASLAbort, successWire)

plainAuth :: ByteString -> ByteString -> SASLAuthenticator ()
plainAuth login password =
  SASLAuthenticator { saslMechanism = "PLAIN"
                    , saslInitial = Just $ login <> "\0" <> password <> "\0"
                    , saslWire = successWire
                    }
