module Machine where

import           Machine.Config
import qualified Machine.Core       as Core

main :: IO ()
main = do
  command <- parseConfig
  case command of
    ARunMachine config -> Core.run config
    AGetVersion version -> putStrLn version
