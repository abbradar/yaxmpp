module Network.XMPP.Presence.Roster
  ( RosterPresenceMap
  , RosterPresenceRef
  , rpresenceSubscribe
  , rpresencePlugin
  ) where

import Data.IORef.Lifted
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Signal (Signal)
import qualified Control.Signal as Signal
import Network.XMPP.Session
import Network.XMPP.Address
import Network.XMPP.Presence
import Network.XMPP.Roster

type RosterPresenceMap = Map BareJID (Map XMPPResource Presence)

data RosterPresenceRef m = RosterPresenceRef { rpresenceRef :: IORef RosterPresenceMap
                                             , rpresenceRoster :: RosterRef m
                                             , rpresenceSignal :: Signal m (FullJID, Maybe Presence)
                                             }

rpresenceHandler :: MonadSession m => RosterPresenceRef m -> PresenceHandler m
rpresenceHandler (RosterPresenceRef {..}) full@(bare, res) presUpd = do
  roster <- rosterEntries <$> rosterGet rpresenceRoster
  if bareJidAddress bare `M.member` roster
    then do
      let updateEntry m = case presUpd of
            Just pres -> Just $ M.insert res pres m
            Nothing ->
              let m' = M.delete res m
              in if M.null m' then Nothing else Just m'
      modifyIORef rpresenceRef $ M.update updateEntry bare
      Signal.emit rpresenceSignal (full, presUpd)
      return True
    else return False

rpresenceSubscribe :: MonadSession m => RosterPresenceRef m -> ((FullJID, Maybe Presence) -> m ()) -> m ()
rpresenceSubscribe (RosterPresenceRef {..}) = Signal.subscribe rpresenceSignal

rpresencePlugin :: MonadSession m => RosterRef m -> m (PresenceHandler m, RosterPresenceRef m)
rpresencePlugin rpresenceRoster = do
  rpresenceRef <- newIORef M.empty
  rpresenceSignal <- Signal.empty
  let pref = RosterPresenceRef {..}
      phandler = rpresenceHandler pref
  return (phandler, pref)
