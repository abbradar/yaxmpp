module Network.XMPP.Connection
       ( Port
       , findServers
       ) where

import Text.Read
import Data.Word
import Data.Maybe
import Data.List
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.Except
import Data.IP
import Network.DNS

type Port = Word16

findServers :: Resolver -> ByteString -> Maybe Port -> ExceptT DNSError IO [(ByteString, Port)]
findServers rsv srvname mport = do
  let defport = fromMaybe 5222 mport
  -- First, check if it is an IP
  let tryRead :: Read a => Maybe a
      tryRead = readMaybe (B.unpack srvname)
  let mip = IPv4 <$> tryRead <|> IPv6 <$> tryRead
  case mip of
    Just _ -> return [(srvname, defport)]
    _ -> do
      -- Try to resolve SRV record
      srvs <- ExceptT $ lookupSRV rsv ("_xmpp-client._tcp." <> srvname)
      -- Default to the server itself if no record found
      return $ if null srvs
        then [(srvname, defport)]
        else map (\(_, _, p, d) -> (B.init d, p)) $
             sortBy (\(p1, w1, _, _) (p2, w2, _, _) -> compare p1 p2 <> compare w1 w2) srvs
