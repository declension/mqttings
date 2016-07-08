module Cli where

import           Options.Applicative
import qualified Network.MQTT as MQTT
import qualified Data.Text as T

data CliOptions = CliOptions {host :: String, topic :: MQTT.Topic, quiet :: Bool}

instance Read MQTT.Topic where
    readsPrec _ str = [(MQTT.toTopic (MQTT.MqttText (T.pack str)), "")]

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> strOption (long "host" <> metavar "HOSTNAME" <> value "localhost" <> help "MQTT broker hostname")
  <*> argument auto      (metavar "TOPIC" <> help "topic to listen to")
  <*> switch             (long "quiet" <> help "Reduce verbosity")
