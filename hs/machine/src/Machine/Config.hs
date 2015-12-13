
module Machine.Config (

    MachineConfig (..)
  , Config (..)
  , parseConfig

  ) where

import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_machine       (version)

data MachineConfig =
  MachineConfig
  { machineTradingAccount :: String
  , machineVenue          :: String
  , machineCoreUrl        :: String
  }
  deriving ( Eq, Ord, Show )

data Config
  = AGetVersion String
  | ARunMachine MachineConfig
  deriving ( Eq, Ord, Show )


parseConfig :: IO Config
parseConfig =
  execParser $ info (helper <*> parser) parserInfo

parserInfo :: InfoMod Config
parserInfo =
  mconcat
  [ progDesc "Stockfighter work system"
  , header ("machine " ++ showVersion version)
  ]

parser :: Parser Config
parser =
  subparser $ mconcat
    [ metavar "COMMAND"
    , command "version" getVersion
    , command "run" (ARunMachine <$> runMachine)
    ]

getVersion :: ParserInfo Config
getVersion = info (helper <*> p) i where
  i = progDesc "get the current version number"
  p = pure (AGetVersion (showVersion version))

runMachine :: ParserInfo MachineConfig
runMachine = info (helper <*> p) i where
  i = progDesc "run the server"
  p = MachineConfig
      <$> option auto tradingAccountOpt
      <*> option auto venueOpt
      <*> option auto coreUrlOpt


  tradingAccountOpt =
    mconcat [ long "account"
            , short 'a'
            , metavar "ACCOUNT_NUMBER"
            , help "what account do you own"
            , value "EXB123456"
            , showDefault
            ]

  venueOpt =
    mconcat [ long "venue"
            , short 'v'
            , metavar "VENUE_NAME"
            , help "what venue are you trading on"
            , value "TESTEX"
            , showDefault
            ]

  coreUrlOpt =
    mconcat [ long "core-url"
            , short 'U'
            , metavar "URL"
            , help "what url should we hit"
            , value "api.stockfighter.io"
            , showDefault
            ]
