{-# Language DataKinds, OverloadedStrings #-}
module Main where

import           Control.Applicative ((<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad (unless, forever)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import           Text.Printf (printf)

import           Options.Applicative (execParser, info, fullDesc, progDesc, helper)
import           Data.Semigroup ((<>))
import qualified Data.ByteString.UTF8 as UTF8
import qualified Network.MQTT as MQTT

import Cli

type TopicName = String

handleMsg :: TopicName  -> MQTT.Message MQTT.PUBLISH -> IO ()
handleMsg topicName msg =
    -- sometimes it's useful to ignore retained messages
    unless (MQTT.retain $ MQTT.header msg) $ do
    let t = MQTT.topic (MQTT.body msg)
        p = MQTT.payload $ MQTT.body msg
    case MQTT.getLevels t of
        [topic, t] -> putStrLn $ printf "%s: %s" (T.unpack t) (UTF8.toString p)
        _ -> hPutStrLn stderr msg
            where msg = printf "Unexpected message on '%s': %s" (show t) (show p)

runApp :: CliOptions -> IO ()
runApp (CliOptions hostname topic isQuiet) = do
    cmds <- MQTT.mkCommands
    pubChan <- newTChanIO
    let conf = (MQTT.defaultConfig cmds pubChan) { MQTT.cHost = hostname }

    _ <- forkIO $ do
        qosGranted <- MQTT.subscribe conf [(topic, MQTT.Handshake)]
        let topicName = (T.unpack . MQTT.text . MQTT.fromTopic) topic
        case qosGranted of
            [MQTT.Handshake] -> do
                unless isQuiet $ putStrLn $ printf "Listening on %s..." topicName
                forever $ atomically (readTChan pubChan) >>= handleMsg topicName
            _ -> do
                hPutStrLn stderr $ "Wanted QoS Handshake, got " ++ show qosGranted
                exitFailure

    -- this will throw IOExceptions
    terminated <- MQTT.run conf
    print terminated


main :: IO ()
main = execParser opts >>= runApp
    where
        opts = info (helper <*> cliOptions)
          (fullDesc <> progDesc "Dumps text traffic on an MQTT topic")
