{-# LANGUAGE RecordWildCards #-}

module Machine.WebSockets ( SecureConfig (..), runSecureClient ) where

import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (toStrict)
import qualified Data.ByteString.Lazy      as BL
import           Network.Connection        (Connection, ConnectionParams (..),
                                            TLSSettings (..), connectTo,
                                            connectionGetChunk, connectionPut,
                                            initConnectionContext)
import           Network.Socket            (PortNumber (..))
import           Network.WebSockets        (ClientApp, ConnectionOptions,
                                            Headers, defaultConnectionOptions,
                                            runClientWithStream)
import           Network.WebSockets.Stream (makeStream)

data SecureConfig =
  SecureConfig
  { swsHost       :: String
  , swsPortNumber :: Int
  , swsPath       :: String
  , swsHeaders    :: Headers
  }

runSecureClient :: SecureConfig -> ClientApp a -> IO a
runSecureClient (SecureConfig { .. }) app = do
  context <- initConnectionContext
  connection <- connectTo context (connectionParams swsHost (fromIntegral swsPortNumber))
  stream <- makeStream (reader connection) (writer connection)
  runClientWithStream stream swsHost swsPath connectionOptions swsHeaders app

connectionParams :: String -> PortNumber -> ConnectionParams
connectionParams host port = ConnectionParams
  { connectionHostname = host
  , connectionPort = port
  , connectionUseSecure = Just tlsSettings
  , connectionUseSocks = Nothing
  }

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
  { settingDisableCertificateValidation = False
  , settingDisableSession = False
  , settingUseServerName = False
  }

reader :: Connection -> IO (Maybe BS.ByteString)
reader connection = fmap Just (connectionGetChunk connection)

writer :: Connection -> Maybe BL.ByteString -> IO ()
writer connection = maybe (return ()) (connectionPut connection . toStrict)

connectionOptions :: ConnectionOptions
connectionOptions = defaultConnectionOptions
