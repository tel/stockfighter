{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Machine.Core where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad
import qualified Data.Aeson         as A
import           Machine.Config
import qualified Machine.Types      as Ty
import           Machine.WebSockets
import qualified Network.WebSockets as Ws

recvJSON :: A.FromJSON a => Ws.Connection -> IO (Either String a)
recvJSON cnx = do
  val <- Ws.receiveDataMessage cnx
  case val of
    Ws.Text s -> return (A.eitherDecode s)
    Ws.Binary _ -> return (Left "binary data received")

run :: MachineConfig -> IO ()
run (MachineConfig { .. }) = do
  let coreWsEndpoint =
        "/ob/api/ws/" ++ machineTradingAccount ++ "/venues/" ++ machineVenue
      tickerTapeEndpoint = coreWsEndpoint ++ "/tickertape"
      _executionsEndpoint = coreWsEndpoint ++ "/executions"

  runSecureClient
    SecureConfig { swsHost = machineCoreUrl
                 , swsPortNumber = 443
                 , swsPath = tickerTapeEndpoint
                 , swsHeaders = []
                 }
    $ \cnx -> do

      putStrLn "client started"

      _ <- forkIO $ forever $ do
        exec <- recvJSON cnx
        putStrLn "msg recv"
        print (exec :: Either String Ty.QuoteWSMessage)

      threadDelay (1000000 * 60)
