import Data.Maybe
import Data.Char
import Control.Monad
import System.Environment
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.Trans.Except
import UnliftIO.Concurrent
import Network.DNS
import Network.Connection
import Control.Monad.Logger
import Data.String.Interpolate (i)
import Data.Conduit.Network
import Data.Conduit
import qualified Data.Conduit.List as C
import Control.Concurrent.STM
import Data.Conduit.TQueue
import qualified Network.IRC as IRC

import Network.XMPP.Connection
import Network.XMPP.Stream
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Plugin
import Network.XMPP.Address
import Network.XMPP.Presence
import Network.XMPP.Presence.Myself
import Network.XMPP.Roster
import Network.XMPP.Message
import Network.XMPP.Language
import Network.XMPP.XEP.Disco
import Network.XMPP.XEP.MUC
import Network.SASL

data Settings = Settings { server :: Text
                         , user :: Text
                         , password :: Text
                         , resource :: Text
                         , conferenceServer :: XMPPDomain
                         , ircPort :: Int
                         }
                deriving (Show, Eq, Generic)

instance JSON.FromJSON Settings where

rpl_WELCOME :: ByteString
rpl_WELCOME = "001"
rpl_TOPIC :: ByteString
rpl_TOPIC = "332"
rpl_NAMREPLY :: ByteString
rpl_NAMREPLY = "353"
rpl_ENDOFNAMES :: ByteString
rpl_ENDOFNAMES = "366"

main :: IO ()
main = runStderrLoggingT $ do
  settingsFile:_ <- liftIO getArgs
  Right settings <- liftIO $ Yaml.decodeFileEither settingsFile

  rs <- liftIO $ makeResolvSeed defaultResolvConf
  Right svrs <- liftIO $ withResolver rs $ \resolver -> runExceptT $ findServers resolver (T.encodeUtf8 $ server settings) Nothing
  $(logInfo) [i|Found servers: #{svrs}|]
  cctx <- liftIO initConnectionContext
  let (host, port) = head svrs
      tsettings = TLSSettingsSimple { settingDisableCertificateValidation = False
                                    , settingDisableSession = False
                                    , settingUseServerName = True
                                    }
      esettings = ConnectionParams { connectionHostname = B.unpack host
                                   , connectionPort = fromIntegral port
                                   , connectionUseSecure = Just tsettings
                                   , connectionUseSocks = Nothing
                                   }
      csettings = ConnectionSettings { connectionParams = esettings
                                     , connectionContext = cctx
                                     , connectionServer = server settings
                                     , connectionUser = user settings
                                     , connectionAuth = [plainAuth "" (T.encodeUtf8 $ user settings) (T.encodeUtf8 $ password settings)]
                                     }
      ssettings = SessionSettings { ssConn = csettings
                                  , ssResource = resource settings
                                  }
      initMain = do
        ms <- sessionCreate ssettings
        case ms of
          Left e -> fail [i|Error creating session: #{e}|]
          Right s -> stanzaSessionCreate s

  bracket initMain (sessionClose . ssSession) $ \sess ->
    bracket (forkIO $ forever $ threadDelay 5000000 >> sessionPeriodic (ssSession sess)) killThread $ \_ -> do
      $(logInfo) "Session successfully created!"
      (myPresH, myPresRef) <- myPresencePlugin sess
      (imP, imRef) <- imPlugin sess
      (mucP, mucPresH, mucDiscoH, mucRef) <- mucPlugin sess
      (rosterP, rosterRef) <- rosterPlugin sess Nothing
      presP <- presencePlugin [myPresH, mucPresH]
      discoP <- discoPlugin [mucDiscoH]

      backQueue <- liftIO newTQueueIO
      let ircReply = liftIO . atomically . writeTQueue backQueue
          conferenceHost = T.encodeUtf8 $ domainText $ conferenceServer settings
          ircServReply cmd args = ircReply $ IRC.Message (Just $ IRC.Server conferenceHost) cmd args
          ircUserReply nick cmd args = ircReply $ IRC.Message (Just $ IRC.NickName nick (Just nick) (Just conferenceHost)) cmd args

      _ <- forkIO $ do
        rst <- rosterGet rosterRef
        $(logInfo) [i|Got initial roster: #{rst}|]
        myPresenceSend myPresRef (Just defaultPresence)

        let processIrcRequest = do
              $(logInfo) [i|New IRC connection|]
              nickVar <- newEmptyMVar
              let getNick = T.encodeUtf8 <$> resourceText <$> readMVar nickVar

              lift $ imSetHandler imRef $ \(addr@(XMPPAddress {..}), msg@(IMMessage {..})) -> if
                | addressDomain == conferenceServer settings -> do
                    let channel = "#" <> T.encodeUtf8 (localText $ fromJust addressLocal)
                        otherNick = T.encodeUtf8 $ T.map (\x -> if x == ' ' then '\xA0' else x) $ resourceText $ fromJust addressResource
                    nick <- getNick
                    unless (nick == otherNick) $ ircUserReply otherNick "PRIVMSG" [channel, T.encodeUtf8 $ T.map (\x -> if isSpace x then ' ' else x) $ localizedGet Nothing imBody]
                | otherwise -> $(logWarn) [i|Got unknown message from #{addr}: #{msg}|]

              lift $ mucSetHandler mucRef $ \case
                MUCJoined jid (MUC {..}) -> do
                  nick <- getNick
                  let channel = "#" <> T.encodeUtf8 (localText $ bareLocal $ fullBare jid)
                      users = B.intercalate " " $ map (T.encodeUtf8 . resourceText) $ M.keys $ mucMembers
                  ircServReply "JOIN" [channel]
                  case mucSubject of
                    Just (_, subj) -> ircServReply rpl_TOPIC [nick, channel, T.encodeUtf8 subj]
                    _ -> return ()
                  ircServReply rpl_NAMREPLY [nick, "*", channel, users]
                  ircServReply rpl_ENDOFNAMES [nick, channel]
                MUCRejected _ _ -> fail "MUC rejected"
                MUCLeft _ _ -> fail "MUC left"

              C.mapM_ $ \req -> do
                $(logDebug) [i|Got IRC request: #{req}|]
                let cmd = IRC.msg_command req
                let params = IRC.msg_params req
                if
                  | cmd == "PING" -> ircServReply "PONG" [conferenceHost]
                  | cmd == "NICK", [newNick] <- params -> do
                      -- FIXME: invalid
                      testNick <- tryReadMVar nickVar
                      when (isNothing testNick) $ putMVar nickVar $ fromJust $ resourceFromText $ T.decodeLatin1 newNick
                  | cmd == "JOIN", [channel] <- params -> do
                      nick0 <- readMVar nickVar
                      let room = fromJust $ localFromText $ T.tail $ T.decodeLatin1 channel
                          joinOpts = defaultMUCJoinSettings { joinHistory = defaultMUCHistorySettings { histMaxStanzas = Just 0 } }
                      handle (\MUCAlreadyJoinedError -> return ()) $ mucJoin mucRef (FullJID (BareJID room $ conferenceServer settings) nick0) joinOpts $ \(MUC {..}) event ->
                        case event of
                          RoomPresence (Added otherNick' _) -> do
                            let otherNick = T.encodeUtf8 $ resourceText otherNick'
                            ircUserReply otherNick "JOIN" [channel]
                          RoomPresence (Removed otherNick' _) -> do
                            let otherNick = T.encodeUtf8 $ resourceText otherNick'
                            ircUserReply otherNick "PART" [channel]
                          RoomSubject -> do
                            nick <- getNick
                            case mucSubject of
                              Just (_, subj) -> ircServReply rpl_TOPIC [nick, channel, T.encodeUtf8 subj]
                              _ -> return ()
                          _ -> return ()
                  | cmd == "USER", (mnick:_) <- params -> do
                      testNick <- tryReadMVar nickVar
                      when (isNothing testNick) $ putMVar nickVar $ fromJust $ resourceFromText $ T.decodeLatin1 mnick
                      nick <- getNick
                      ircServReply rpl_WELCOME [nick, "Welcome to the Internet Relay Network " <> nick]
                  | cmd == "PRIVMSG", [channel, msg'] <- params -> do
                      let room = fromJust $ localFromText $ T.tail $ T.decodeLatin1 channel
                          msg = fromMaybe msg' $ fmap (\x -> "/me" <> B.init x) $ B.stripPrefix "\SOHACTION" msg'
                          msgText = T.decodeUtf8 msg
                          imMsg = IMMessage { imType = MessageGroupchat
                                            , imSubject = Nothing
                                            , imBody = localizedFromText msgText
                                            , imThread = Nothing
                                            , imExtended = []
                                            }
                      imSend imRef (XMPPAddress (Just room) (conferenceServer settings) Nothing) imMsg
                  | otherwise -> $(logWarn) [i|Unknown IRC command: #{req}|]

        runGeneralTCPServer (serverSettings (ircPort settings) "*") $ \app -> do
          let clearReply rep = do
                $(logDebug) [i|Sending IRC message: #{rep}|]
                return (rep <> "\r\n")
              
              resplit msg old = (last msgs, map (<> "\n") $ init msgs)
                where msgs = B.split '\n' (old <> msg)

              parseMsg dat = do
                let res = IRC.parseMessage dat
                when (isNothing res) $ $(logWarn) [i|Failed to parse IRC message: #{dat}|]
                return res

          _ <- forkIO $ runConduit $ sourceTQueue backQueue .| C.map IRC.encode .| C.mapM clearReply .| appSink app
          runConduit $ appSource app .| C.concatMapAccum resplit "" .| C.mapMaybeM parseMsg .| processIrcRequest
  
      let plugins = [presP, imP, discoP, mucP, rosterP]
      forever $ stanzaSessionStep sess (pluginsInHandler plugins) (pluginsRequestIqHandler plugins)
