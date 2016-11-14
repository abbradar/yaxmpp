module Network.XMPP.Presence.Roster
  ( RosterPresenceMap
  , RosterPresenceRef
  , rpresenceSetHandler
  , rpresencePlugin
  ) where

import Data.IORef.Lifted
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
  
import Control.Handler (Handler)
import qualified Control.Handler as Handler
import Network.XMPP.Session
import Network.XMPP.Address
import Network.XMPP.Presence
import Network.XMPP.Roster

type RosterPresenceMap = Map BareJID (Map XMPPResource Presence)

data RosterPresenceRef m = RosterPresenceRef { rpresenceRef :: IORef RosterPresenceMap
                                             , rpresenceRoster :: RosterRef m
                                             , rpresenceHandler :: Handler m (FullJID, Maybe Presence)
                                             }

rpresencePHandler :: MonadSession m => RosterPresenceRef m -> PresenceHandler m
rpresencePHandler (RosterPresenceRef {..}) full@(FullJID {..}) presUpd = do
  roster <- rosterEntries <$> rosterGet rpresenceRoster
  if bareJidAddress fullBare `M.member` roster
    then do
      let updateEntry m = case presUpd of
            Just pres -> Just $ M.insert fullResource pres m
            Nothing ->
              let m' = M.delete fullResource m
              in if M.null m' then Nothing else Just m'
      modifyIORef rpresenceRef $ M.update updateEntry fullBare
      Handler.call rpresenceHandler (full, presUpd)
      return True
    else return False

rpresenceSetHandler :: MonadSession m => RosterPresenceRef m -> ((FullJID, Maybe Presence) -> m ()) -> m ()
rpresenceSetHandler (RosterPresenceRef {..}) = Handler.set rpresenceHandler

rpresencePlugin :: MonadSession m => RosterRef m ->  m (PresenceHandler m, RosterPresenceRef m)
rpresencePlugin rpresenceRoster = do
  rpresenceRef <- newIORef M.empty
  rpresenceHandler <- Handler.new
  let pref = RosterPresenceRef {..}
      phandler = rpresencePHandler pref
  return (phandler, pref)
