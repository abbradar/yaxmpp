import Control.Concurrent.Linked
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Slot (SlotSignal (..))
import qualified Control.Slot as Slot
import qualified Data.Aeson as JSON
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Conduit.Network
import Data.Conduit.TQueue
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Network.Connection
import Network.DNS
import qualified Network.IRC as IRC
import Network.TLS (EMSMode (..), Supported (..))
import System.Environment
import UnliftIO (liftIO)
import UnliftIO.Concurrent

import qualified Data.Registry as Reg
import Network.SASL
import Network.XMPP.Address
import Network.XMPP.Connection
import Network.XMPP.Language
import Network.XMPP.Message
import Network.XMPP.Plugin
import Network.XMPP.Presence
import Network.XMPP.Presence.Myself
import Network.XMPP.Roster
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.XEP.Capabilities
import Network.XMPP.XEP.DelayedDelivery
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.Disco.HomeCache
import Network.XMPP.XEP.Disco.PresenceCache
import Network.XMPP.XEP.MUC

data Settings = Settings
  { server :: Text
  , user :: Text
  , password :: Text
  , resource :: Text
  , conferenceServer :: XMPPDomain
  , ircPort :: Int
  }
  deriving (Show, Eq, Generic)

instance JSON.FromJSON Settings

data IRCBridgePlugin m = IRCBridgePlugin
  { ircConferenceServer :: XMPPDomain
  , ircGetNick :: m ByteString
  , ircServReply :: ByteString -> [ByteString] -> m ()
  , ircUserReply :: ByteString -> ByteString -> [ByteString] -> m ()
  }

instance (MonadStream m) => SlotSignal m AddressedIMMessage (IRCBridgePlugin m) where
  emitSignal (IRCBridgePlugin {..}) AddressedIMMessage {imFrom = addr@(XMPPAddress {..}), imMessage = IMMessage {..}} =
    if addressDomain == ircConferenceServer
      then do
        let channel = "#" <> T.encodeUtf8 (localText $ fromJust addressLocal)
            otherNick = T.encodeUtf8 $ T.map (\x -> if x == ' ' then '\xA0' else x) $ resourceText $ fromJust addressResource
        nick <- ircGetNick
        unless (nick == otherNick) $ ircUserReply otherNick "PRIVMSG" [channel, T.encodeUtf8 $ T.map (\x -> if isSpace x then ' ' else x) $ localizedGet Nothing imBody]
      else $(logWarn) [i|Got unknown message from #{addr}: #{imBody}|]

instance (MonadStream m) => SlotSignal m MUCEvent (IRCBridgePlugin m) where
  emitSignal (IRCBridgePlugin {..}) = \case
    MUCJoinedRoom jid (MUC {..}) -> do
      nick <- ircGetNick
      let channel = "#" <> T.encodeUtf8 (localText $ bareLocal $ fullBare jid)
          users = B.intercalate " " $ map (T.encodeUtf8 . resourceText) $ M.keys mucMembers
      ircServReply "JOIN" [channel]
      case mucSubject of
        Just (_, subj) -> ircServReply rplTOPIC [nick, channel, T.encodeUtf8 subj]
        _ -> return ()
      ircServReply rplNAMREPLY [nick, "*", channel, users]
      ircServReply rplENDOFNAMES [nick, channel]
    MUCRejected _ _ -> fail "MUC rejected"
    MUCLeftRoom _ _ -> fail "MUC left"

rplWELCOME :: ByteString
rplWELCOME = "001"
rplTOPIC :: ByteString
rplTOPIC = "332"
rplNAMREPLY :: ByteString
rplNAMREPLY = "353"
rplENDOFNAMES :: ByteString
rplENDOFNAMES = "366"

main :: IO ()
main = runStderrLoggingT $ do
  settingsFile : _ <- liftIO getArgs
  Right settings <- liftIO $ Yaml.decodeFileEither settingsFile

  rs <- liftIO $ makeResolvSeed defaultResolvConf
  Right svrs <- liftIO $ withResolver rs $ \resolver -> runExceptT $ findServers resolver (T.encodeUtf8 $ server settings) Nothing
  $(logInfo) [i|Found servers: #{svrs}|]
  cctx <- liftIO initConnectionContext
  (host, port) : _ <- pure svrs
  let tsettings =
        TLSSettingsSimple
          { settingDisableCertificateValidation = False
          , settingDisableSession = False
          , settingUseServerName = True
          , settingClientSupported = def {supportedExtendedMainSecret = AllowEMS}
          }
      esettings =
        ConnectionParams
          { connectionHostname = B.unpack host
          , connectionPort = fromIntegral port
          , connectionUseSecure = Just tsettings
          , connectionUseSocks = Nothing
          }
      csettings =
        ConnectionSettings
          { connectionParams = esettings
          , connectionContext = cctx
          , connectionServer = server settings
          , connectionUser = user settings
          , connectionAuth = [plainAuth "" (T.encodeUtf8 $ user settings) (T.encodeUtf8 $ password settings)]
          }
      ssettings =
        SessionSettings
          { ssConn = csettings
          , ssResource = resource settings
          }
      initMain = do
        ms <- sessionCreate ssettings
        case ms of
          Left e -> fail [i|Error creating session: #{e}|]
          Right s -> stanzaSessionCreate s

  bracket initMain (sessionClose . ssSession) $ \sess ->
    bracket (forkLinked $ forever $ threadDelay 5000000 >> sessionPeriodic (ssSession sess)) killThread $ \_ -> do
      $(logInfo) "Session successfully created!"
      pluginsRef <- newXmppPlugins sess Nothing
      presencePlugin pluginsRef
      capsPlugin pluginsRef
      discoPlugin pluginsRef
      homeCachePlugin pluginsRef
      presenceCachePlugin pluginsRef
      rosterPlugin pluginsRef
      myPresencePlugin pluginsRef
      imPlugin pluginsRef
      delayedDeliveryPlugin pluginsRef
      mucPlugin pluginsRef

      backQueue <- liftIO newTQueueIO
      let ircReply = liftIO . atomically . writeTQueue backQueue
          conferenceHost = T.encodeUtf8 $ domainText $ conferenceServer settings
          ircServReply cmd args = ircReply $ IRC.Message (Just $ IRC.Server conferenceHost) cmd args
          ircUserReply nick cmd args = ircReply $ IRC.Message (Just $ IRC.NickName nick (Just nick) (Just conferenceHost)) cmd args

      rosterP <- getRosterPlugin pluginsRef
      myPresP <- getMyPresencePlugin pluginsRef
      imP <- getIMPlugin pluginsRef
      mucP <- getMUCPlugin pluginsRef

      _ <- forkLinked $ do
        rst <- getRoster rosterP
        $(logInfo) [i|Got initial roster: #{rst}|]
        myPresenceSend myPresP (Just defaultPresence)

        let processIrcRequest = do
              $(logInfo) [i|New IRC connection|]
              nickVar <- newEmptyMVar
              let getNick = T.encodeUtf8 . resourceText <$> readMVar nickVar

              let bridgePlugin =
                    IRCBridgePlugin
                      { ircConferenceServer = conferenceServer settings
                      , ircGetNick = getNick
                      , ircServReply = ircServReply
                      , ircUserReply = ircUserReply
                      }
              lift $ Slot.pushNewOrFailM bridgePlugin (imPluginSlot imP)
              lift $ Slot.pushNewOrFailM bridgePlugin (mucPluginSlot mucP)

              C.mapM_ $ \req -> do
                $(logDebug) [i|Got IRC request: #{req}|]
                let cmd = IRC.msg_command req
                let params = IRC.msg_params req
                if
                  | cmd == "PING" -> ircServReply "PONG" [conferenceHost]
                  | cmd == "NICK"
                  , [newNick] <- params -> do
                      -- FIXME: invalid
                      testNick <- tryReadMVar nickVar
                      when (isNothing testNick) $ putMVar nickVar $ fromJust $ resourceFromText $ T.decodeLatin1 newNick
                  | cmd == "JOIN"
                  , [channel] <- params -> do
                      nick0 <- readMVar nickVar
                      let room = fromJust $ localFromText $ T.tail $ T.decodeLatin1 channel
                          joinOpts = defaultMUCJoinSettings {joinHistory = defaultMUCHistorySettings {histMaxStanzas = Just 0}}
                      handle (\MUCAlreadyJoinedError -> return ()) $
                        mucJoin
                          mucP
                          (FullJID (BareJID room $ conferenceServer settings) nick0)
                          joinOpts
                          ( \(MUC {..}) event ->
                              case event of
                                RoomPresence otherNick' (MUCJoined _) -> do
                                  let otherNick = T.encodeUtf8 $ resourceText otherNick'
                                  ircUserReply otherNick "JOIN" [channel]
                                RoomPresence otherNick' (MUCRemoved _) -> do
                                  let otherNick = T.encodeUtf8 $ resourceText otherNick'
                                  ircUserReply otherNick "PART" [channel]
                                RoomSubject -> do
                                  nick <- getNick
                                  case mucSubject of
                                    Just (_, subj) -> ircServReply rplTOPIC [nick, channel, T.encodeUtf8 subj]
                                    _ -> return ()
                                _ -> return ()
                          )
                          (\_ -> return ())
                  | cmd == "USER"
                  , (mnick : _) <- params -> do
                      testNick <- tryReadMVar nickVar
                      when (isNothing testNick) $ putMVar nickVar $ fromJust $ resourceFromText $ T.decodeLatin1 mnick
                      nick <- getNick
                      ircServReply rplWELCOME [nick, "Welcome to the Internet Relay Network " <> nick]
                  | cmd == "PRIVMSG"
                  , [channel, msg'] <- params -> do
                      let room = fromJust $ localFromText $ T.tail $ T.decodeLatin1 channel
                          msg = maybe msg' (\x -> "/me" <> B.init x) $ B.stripPrefix "\SOHACTION" msg'
                          msgText = T.decodeUtf8 msg
                          imMsg =
                            IMMessage
                              { imId = Nothing
                              , imType = MessageGroupchat
                              , imSubject = Nothing
                              , imBody = localizedFromText msgText
                              , imThread = Nothing
                              , imRaw = []
                              , imExtended = Reg.empty
                              }
                      void $ imSend imP (XMPPAddress (Just room) (conferenceServer settings) Nothing) imMsg
                  | otherwise -> $(logWarn) [i|Unknown IRC command: #{req}|]

        runGeneralTCPServer (serverSettings (ircPort settings) "*") $ \app -> do
          let clearReply rep = do
                $(logDebug) [i|Sending IRC message: #{rep}|]
                return (rep <> "\r\n")

              resplit msg old = (last msgs, map (<> "\n") $ init msgs)
               where
                msgs = B.split '\n' (old <> msg)

              parseMsg dat = do
                let res = IRC.parseMessage dat
                when (isNothing res) $ $(logWarn) [i|Failed to parse IRC message: #{dat}|]
                return res

          _ <- forkLinked $ runConduit $ sourceTQueue backQueue .| C.map IRC.encode .| C.mapM clearReply .| appSink app
          runConduit $ appSource app .| C.concatMapAccum resplit "" .| C.mapMaybeM parseMsg .| processIrcRequest

      forever $ pluginsSessionStep pluginsRef
