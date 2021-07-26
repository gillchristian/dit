module CLI (run, Command (..), Options (..)) where

import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt

todoOpts :: Opt.ParserInfo Command
todoOpts =
  Opt.info (pure Todo) (Opt.progDesc desc)
  where
    desc = "What am I missing to do today?"

logOpts :: Opt.ParserInfo Command
logOpts =
  Log <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Your progress"
    opts =
      LogOptions
        <$> Opt.option
          Opt.auto
          ( Opt.long "days"
              <> Opt.short 'd'
              <> Opt.metavar "DAYS"
              <> Opt.help "How many days back to show"
              <> Opt.value 7
              <> Opt.showDefault
          )

-- TODO move to the module that runs this action ?
newtype LogOptions = LogOptions
  {logDays :: Int}
  deriving (Eq, Show)

data Command
  = Todo
  | Log LogOptions
  deriving (Eq, Show)

data Options = Options
  { optsCommand :: Command,
    optsConfig :: Maybe FilePath
  }
  deriving (Eq, Show)

{-
  $ dit --config ~/.config/dit.toml todo
  $ dit --config ~/.config/dit.toml log
  $ dit --config ~/.config/dit.toml log --days 14
-}

options :: Opt.ParserInfo Options
options =
  Opt.info
    (programm <**> Opt.helper)
    (Opt.fullDesc <> Opt.header "Show up everyday and do it!")
  where
    programm =
      Options
        <$> Opt.hsubparser (todo <> log)
        <*> confArg
    todo = Opt.command "todo" todoOpts
    log = Opt.command "log" logOpts
    confArg =
      Opt.optional
        ( Opt.strOption
            ( Opt.long "config"
                <> Opt.short 'c'
                <> Opt.metavar "CONFIG"
                <> Opt.help "Config file path"
                <> Opt.showDefault
            )
        )

run :: IO Options
run = Opt.execParser options
