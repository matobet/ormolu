{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad
import Data.Bool (bool)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, replace, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Development.GitRev
import GHC.Types.Fixity (FixityDirection (..))
import Options.Applicative
import Ormolu
import Ormolu.Diff.Text (diffText, printTextDiff)
import Ormolu.Fixity
  ( ConflictStrategy (..),
    FixityConfig (..),
    FixityInfo (..),
    PackageConfig (..),
    defaultConflictStrategy,
  )
import Ormolu.Parser (manualExts)
import Ormolu.Terminal
import Ormolu.Utils (showOutputable)
import Ormolu.Utils.Cabal (getCabalDependencies, getCabalExtensionDynOptions)
import Ormolu.Utils.IO
import Paths_ormolu (version)
import System.Exit (ExitCode (..), exitWith)
import qualified System.FilePath as FP
import System.IO (hPutStrLn, stderr)
import Text.Regex.Pcre2 (capture, regex)

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  let formatOne' =
        formatOne
          optCabalDefaultExtensions
          optMode
          optSourceType
          optConfig
  exitCode <- case optInputFiles of
    [] -> formatOne' Nothing
    ["-"] -> formatOne' Nothing
    [x] -> formatOne' (Just x)
    xs -> do
      let selectFailure = \case
            ExitSuccess -> Nothing
            ExitFailure n -> Just n
      errorCodes <-
        mapMaybe selectFailure <$> mapM (formatOne' . Just) (sort xs)
      return $
        if null errorCodes
          then ExitSuccess
          else
            ExitFailure $
              if all (== 100) errorCodes
                then 100
                else 102
  exitWith exitCode

-- | Format a single input.
formatOne ::
  -- | How to use .cabal files
  CabalOpts ->
  -- | Mode of operation
  Mode ->
  -- | The 'SourceType' requested by the user
  Maybe SourceType ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ExitCode
formatOne CabalOpts {..} mode reqSourceType rawConfig mpath =
  withPrettyOrmoluExceptions (cfgColorMode rawConfig) $ do
    case FP.normalise <$> mpath of
      -- input source = STDIN
      Nothing -> do
        resultConfig <-
          if optUseCabalDefaultExtensions || optUseCabalDependencies
            then case optStdinInputFile of
              Just stdinInputFile ->
                patchConfig Nothing
                  <$> ( if optUseCabalDefaultExtensions
                          then getCabalExtensionDynOptions stdinInputFile
                          else pure []
                      )
                  <*> ( if optUseCabalDependencies
                          then getCabalDependencies stdinInputFile
                          else pure []
                      )
              Nothing -> throwIO $ OrmoluMissingStdinInputFile
            else pure $ patchConfig Nothing [] []
        case mode of
          Stdout -> do
            ormoluStdin resultConfig >>= TIO.putStr
            return ExitSuccess
          InPlace -> do
            hPutStrLn
              stderr
              "In place editing is not supported when input comes from stdin."
            -- 101 is different from all the other exit codes we already use.
            return (ExitFailure 101)
          Check -> do
            -- ormoluStdin is not used because we need the originalInput
            originalInput <- getContentsUtf8
            let stdinRepr = "<stdin>"
            formattedInput <-
              ormolu resultConfig stdinRepr (T.unpack originalInput)
            handleDiff originalInput formattedInput stdinRepr
      -- input source = a file
      Just inputFile -> do
        resultConfig <-
          patchConfig (Just (detectSourceType inputFile))
            <$> ( if optUseCabalDefaultExtensions
                    then getCabalExtensionDynOptions inputFile
                    else pure []
                )
            <*> ( if optUseCabalDependencies
                    then getCabalDependencies inputFile
                    else pure []
                )
        case mode of
          Stdout -> do
            ormoluFile resultConfig inputFile >>= TIO.putStr
            return ExitSuccess
          InPlace -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- readFileUtf8 inputFile
            formattedInput <-
              ormolu resultConfig inputFile (T.unpack originalInput)
            when (formattedInput /= originalInput) $
              writeFileUtf8 inputFile formattedInput
            return ExitSuccess
          Check -> do
            -- ormoluFile is not used because we need originalInput
            originalInput <- readFileUtf8 inputFile
            formattedInput <-
              ormolu resultConfig inputFile (T.unpack originalInput)
            handleDiff originalInput formattedInput inputFile
  where
    patchConfig mdetectedSourceType dynOpts cabalDependencies =
      rawConfig
        { cfgDynOptions = cfgDynOptions rawConfig ++ dynOpts,
          cfgSourceType =
            fromMaybe
              ModuleSource
              (reqSourceType <|> mdetectedSourceType),
          cfgFixityConfig =
            (cfgFixityConfig rawConfig)
              { fcDetectedCabalDependencies = cabalDependencies
              }
        }
    handleDiff originalInput formattedInput fileRepr =
      case diffText originalInput formattedInput fileRepr of
        Nothing -> return ExitSuccess
        Just diff -> do
          runTerm (printTextDiff diff) (cfgColorMode rawConfig) stderr
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          return (ExitFailure 100)

----------------------------------------------------------------------------
-- Command line options parsing

data Opts = Opts
  { -- | Mode of operation
    optMode :: !Mode,
    -- | Ormolu 'Config'
    optConfig :: !(Config RegionIndices),
    -- | Options for respecting default-extensions from .cabal files
    optCabalDefaultExtensions :: CabalOpts,
    -- | Source type option, where 'Nothing' means autodetection
    optSourceType :: !(Maybe SourceType),
    -- | Haskell source files to format or stdin (when the list is empty)
    optInputFiles :: ![FilePath]
  }

-- | Mode of operation.
data Mode
  = -- | Output formatted source code to stdout
    Stdout
  | -- | Overwrite original file
    InPlace
  | -- | Exit with non-zero status code if
    -- source is not already formatted
    Check
  deriving (Eq, Show)

-- | Configuration for how to use .cabal files
data CabalOpts = CabalOpts
  { -- | Account for default-extensions from .cabal files
    optUseCabalDefaultExtensions :: Bool,
    -- | Use dependencies listed in .cabal files to infer operator fixities
    optUseCabalDependencies :: Bool,
    -- | Optional path to a file which will be used to
    -- find a .cabal file when using input from stdin
    optStdinInputFile :: Maybe FilePath
  }
  deriving (Show)

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> exts <*> optsParser) . mconcat $
    [fullDesc]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      intercalate
        "\n"
        [ unwords
            [ "ormolu",
              showVersion version,
              $gitBranch,
              $gitHash
            ],
          "using ghc-lib-parser " ++ "9.2.1.20211101"
        ]
    exts :: Parser (a -> a)
    exts =
      infoOption displayExts . mconcat $
        [ long "manual-exts",
          help "Display extensions that need to be enabled manually"
        ]
    displayExts = unlines $ sort (showOutputable <$> manualExts)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( (fmap (bool Stdout InPlace) . switch . mconcat)
            [ short 'i',
              help "A shortcut for --mode inplace"
            ]
            <|> (option parseMode . mconcat)
              [ long "mode",
                short 'm',
                metavar "MODE",
                value Stdout,
                help "Mode of operation: 'stdout' (the default), 'inplace', or 'check'"
              ]
        )
    <*> configParser
    <*> cabalOptsParser
    <*> sourceTypeParser
    <*> (many . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source files to format or stdin (the default)"
      ]

cabalOptsParser :: Parser CabalOpts
cabalOptsParser =
  CabalOpts
    <$> (switch . mconcat)
      [ short 'e',
        long "cabal-default-extensions",
        help "Account for default-extensions from .cabal files"
      ]
    <*> ( (flag' False . mconcat)
            [ long "no-cabal-dependencies",
              help "Do not use dependencies listed in .cabal files to infer operator fixities"
            ]
            <|> (flag' True . mconcat)
              [ long "cabal-dependencies",
                help "Use dependencies listed in .cabal files to infer operator fixities (default)"
              ]
            <|> pure True
        )
    <*> (optional . strOption . mconcat)
      [ long "stdin-input-file",
        help "Path which will be used to find the .cabal file when using input from stdin"
      ]

configParser :: Parser (Config RegionIndices)
configParser =
  Config
    <$> (fmap (fmap DynOption) . many . strOption . mconcat)
      [ long "ghc-opt",
        short 'o',
        metavar "OPT",
        help "GHC options to enable (e.g. language extensions)"
      ]
    <*> (switch . mconcat)
      [ long "unsafe",
        short 'u',
        help "Do formatting faster but without automatic detection of defects"
      ]
    <*> (switch . mconcat)
      [ long "debug",
        short 'd',
        help "Output information useful for debugging"
      ]
    <*> (switch . mconcat)
      [ long "check-idempotence",
        short 'c',
        help "Fail if formatting is not idempotent"
      ]
    -- We cannot parse the source type here, because we might need to do
    -- autodection based on the input file extension (not available here)
    -- before storing the resolved value in the config struct.
    <*> pure ModuleSource
    <*> (option parseColorMode . mconcat)
      [ long "color",
        metavar "WHEN",
        value Auto,
        help "Colorize the output; WHEN can be 'never', 'always', or 'auto' (the default)"
      ]
    <*> ( RegionIndices
            <$> (optional . option auto . mconcat)
              [ long "start-line",
                metavar "START",
                help "Start line of the region to format (starts from 1)"
              ]
            <*> (optional . option auto . mconcat)
              [ long "end-line",
                metavar "END",
                help "End line of the region to format (inclusive)"
              ]
        )
    <*> ( FixityConfig
            <$> ( fmap HashMap.fromList . many $
                    (option parseFcOpManualConfig . mconcat)
                      [ long "fixconf",
                        metavar "FIX_DECL",
                        help "Operator fixity declaration (examples: --fixconf ':>'=infixr-0 --fixconf ':=>'=0-1 -fixconf ':$>'=infix-2-9 )"
                      ]
                )
            <*> ( many $
                    (option parseFcPackageManualConfig . mconcat)
                      [ long "fixconf-from-package",
                        metavar "PKG_FIX_DECL",
                        help "Packages from which operator declarations will be whitelisted or excluded (examples: --fixconf-from-package containers --fixconf-from-package base=all-'$'-'+' --fixconf-from-package servant=none --fixconf-from-package text=none+':>' )"
                      ]
                )
            -- will be populated later
            <*> pure []
            <*> ( (flag' Nothing . mconcat)
                    [ long "no-fixconf-from-hoogle-hackage",
                      help "Do not use the Hoogle/Hackage database to infer operator fixities not specified with the other options"
                    ]
                    <|> (option (fmap Just parseFcConflictStrategy) . mconcat)
                      [ long "fixconf-from-hoogle-hackage",
                        metavar "CONFLICT_STRATEGY",
                        help "Use the Hoogle/Hackage database to infer operator fixities not specified with the other options, using the specified strategy to resolve conflicting declarations (keep-best, merge-all, or use-threshold=FLOAT with FLOAT in [0,1])"
                      ]
                    <|> pure (Just defaultConflictStrategy)
                )
        )

sourceTypeParser :: Parser (Maybe SourceType)
sourceTypeParser =
  (option parseSourceType . mconcat)
    [ long "source-type",
      short 't',
      metavar "TYPE",
      value Nothing,
      help "Set the type of source; TYPE can be 'module', 'sig', or 'auto' (the default)"
    ]

----------------------------------------------------------------------------
-- Helpers

-- | Parse 'Mode'.
parseMode :: ReadM Mode
parseMode = eitherReader $ \case
  "stdout" -> Right Stdout
  "inplace" -> Right InPlace
  "check" -> Right Check
  s -> Left $ "unknown mode: " ++ s

-- | Parse 'ColorMode'.
parseColorMode :: ReadM ColorMode
parseColorMode = eitherReader $ \case
  "never" -> Right Never
  "always" -> Right Always
  "auto" -> Right Auto
  s -> Left $ "unknown color mode: " ++ s

-- | Parse the 'SourceType'. 'Nothing' means that autodetection based on
-- file extension is requested.
parseSourceType :: ReadM (Maybe SourceType)
parseSourceType = eitherReader $ \case
  "module" -> Right (Just ModuleSource)
  "sig" -> Right (Just SignatureSource)
  "auto" -> Right Nothing
  s -> Left $ "unknown source type: " ++ s

unescape :: Text -> Text
unescape = replace (pack "\\'") (pack "'") . replace (pack "\\\\") (pack "\\")

-- syntax: --fixconf ':>'=infixr-0    -> (":>", FixityInfo (Just InfixR) 0 0)
--         --fixconf ':>'=0-1        -> (":>", FixityInfo Nothing 0 1)
--         --fixconf ':>'=infixl-2-9  -> (":>", FixityInfo (Just InfixL) 2 9)
parseFcOpManualConfig :: ReadM (String, FixityInfo)
parseFcOpManualConfig = eitherReader $ \s ->
  case [regex|^'(?<opName>(?:\\\\|\\'|[^\\'])+?)'=(?:(?<fixDir>infix[rl]?)-)?(?<fixPrec>[0-9])(?:-(?<fixMaxPrec>[0-9]))?$|] (pack s) of
    Just cs ->
      let (opName, rawFixDir, rawFixPrec, rawFixMaxPrec) =
            ( unpack . unescape $ capture @"opName" cs,
              unpack $ capture @"fixDir" cs,
              unpack $ capture @"fixPrec" cs,
              unpack $ capture @"fixMaxPrec" cs
            )
          fixDir = case rawFixDir of
            "" -> Nothing
            "infixr" -> Just InfixR
            "infixl" -> Just InfixL
            "infix" -> Just InfixN
            other ->
              error $
                "Invalid fixity direction (fix the regex): " ++ other
          fixMinPrec = read rawFixPrec :: Int
          fixMaxPrec =
            if null rawFixMaxPrec
              then fixMinPrec
              else read rawFixMaxPrec
       in Right (opName, FixityInfo {fixDir, fixMinPrec, fixMaxPrec})
    Nothing -> Left $ "Invalid fixity config format: " ++ s

-- syntax: --fixconf-from-package containers                 -> ("containers", AllExcept [])
--         --fixconf-from-package containers=all-':>'-':=>'  -> ("containers", AllExcept [":>", ":=>"])
--         --fixconf-from-package containers=none            -> ("containers", NoneExcept [])
--         --fixconf-from-package containers=none+':>'       -> ("containers", NoneExcept [":>"])
parseFcPackageManualConfig :: ReadM (String, PackageConfig)
parseFcPackageManualConfig = eitherReader $ \s ->
  case [regex|^(?<packageName>[A-Za-z0-9-_]+?)(?:=all(?<allExcept>(?:-'(?:\\\\|\\'|[^\\'])+?')*?)|(?<modeNone>=none(?<noneExcept>(?:\+'(?:\\\\|\\'|[^\\'])+?')*?)))?$|] (pack s) of
    Just cs ->
      let (packageName, rawAllExceptList, modeNone, rawNoneExceptList) =
            ( unpack $ capture @"packageName" cs,
              capture @"allExcept" cs,
              not . T.null $ capture @"modeNone" cs,
              capture @"noneExcept" cs
            )
          packageConfig =
            if modeNone
              then NoneExcept $ getNoneExceptList rawNoneExceptList
              else AllExcept $ getAllExceptList rawAllExceptList
       in Right (packageName, packageConfig)
    Nothing -> Left $ "Invalid fixity config format: " ++ s
  where
    getNoneExceptList txt =
      case [regex|^(?<opName>\+'(?:\\\\|\\'|[^\\'])+?')(?<remaining>.*)$|] txt of
        Just cs ->
          let (opName, remaining) =
                ( unpack . unescape $ capture @"opName" cs,
                  capture @"remaining" cs
                )
           in opName : getNoneExceptList remaining
        Nothing ->
          if T.null txt
            then []
            else error $ "Unconsumed data: " ++ unpack txt
    getAllExceptList txt =
      case [regex|^(?<opName>-'(?:\\\\|\\'|[^\\'])+?')(?<remaining>.*)$|] txt of
        Just cs ->
          let (opName, remaining) =
                ( unpack . unescape $ capture @"opName" cs,
                  capture @"remaining" cs
                )
           in opName : getAllExceptList remaining
        Nothing ->
          if T.null txt
            then []
            else error $ "Unconsumed data: " ++ unpack txt

-- syntax: keep-best | merge-all | use-threshold=float
parseFcConflictStrategy :: ReadM ConflictStrategy
parseFcConflictStrategy = eitherReader $ \s ->
  case [regex|^(?<strategy>keep-best|merge-all|use-threshold=(?<threshold>(?:0\.?|1\.?|0?\.[0-9]+?|1\.0+?)))$|] (pack s) of
    Just cs ->
      let (strategy, rawThreshold) =
            ( unpack $ capture @"strategy" cs,
              unpack $ capture @"threshold" cs
            )
       in Right $
            if not (null rawThreshold)
              then UseThreshold (read rawThreshold)
              else case strategy of
                "keep-best" -> KeepBest
                "merge-all" -> MergeAll
                other -> error $ "Invalid strategy (fix the regex): " ++ other
    Nothing -> Left $ "Invalid strategy: " ++ s
