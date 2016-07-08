{-# Language DataKinds, OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless, forever)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import Text.Printf (printf)
import qualified Data.ByteString.UTF8 as UTF8

import qualified Network.MQTT as MQTT

topic:: MQTT.Topic
-- Note '#' is a wildcard matching any number of topics
topic = "quodlibet/#"

handleMsg :: MQTT.Message MQTT.PUBLISH -> IO ()
handleMsg msg =
    -- sometimes it's useful to ignore retained messages
    unless (MQTT.retain $ MQTT.header msg) $ do
    let t = MQTT.topic (MQTT.body msg)
        p = MQTT.payload $ MQTT.body msg
    case MQTT.getLevels t of
        ["quodlibet", t] -> putStrLn $ printf "%s: %s" (T.unpack t) (UTF8.toString p)
        _ -> hPutStrLn stderr msg
            where msg = printf "Unexpected message on '%s': %s" (show t) (show p)

main :: IO ()
main = do
    cmds <- MQTT.mkCommands
    pubChan <- newTChanIO
    let conf = (MQTT.defaultConfig cmds pubChan) { MQTT.cHost = "nas" }

    _ <- forkIO $ do
        qosGranted <- MQTT.subscribe conf [(topic, MQTT.Handshake)]
        case qosGranted of
            [MQTT.Handshake] -> do
                putStrLn $ printf "Listening on %s..." (MQTT.text $ MQTT.fromTopic topic)
                forever $ atomically (readTChan pubChan) >>= handleMsg
            _ -> do
                hPutStrLn stderr $ "Wanted QoS Handshake, got " ++ show qosGranted
                exitFailure

    -- this will throw IOExceptions
    terminated <- MQTT.run conf
    print terminated
