{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Parser for Haskell source code.
module Ormolu.Parser
  ( parseModule,
    manualExts,
  )
where

import Control.Exception
import Control.Monad.Except
import Data.Char (isSpace)
import Data.Functor
import Data.Generics
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down (Down))
import GHC.Data.Bag (bagToList)
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.CmdLine as GHC
import GHC.Driver.Session as GHC
import GHC.DynFlags (baseDynFlags)
import GHC.Hs hiding (UnicodeSyntax)
import GHC.LanguageExtensions.Type (Extension (..))
import qualified GHC.Parser as GHC
import GHC.Parser.Errors.Ppr (pprError)
import qualified GHC.Parser.Header as GHC
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Types.SourceError as GHC (handleSourceError)
import GHC.Types.SrcLoc
import GHC.Utils.Error (Severity (..), errMsgSeverity, errMsgSpan)
import qualified GHC.Utils.Panic as GHC
import Ormolu.Config
import Ormolu.Exception
import Ormolu.Fixity (LazyFixityMap)
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Processing.Common
import Ormolu.Processing.Preprocess
import Ormolu.Utils (incSpanLine)

-- | Parse a complete module from string.
parseModule ::
  MonadIO m =>
  -- | Ormolu configuration
  Config RegionDeltas ->
  -- | Fixity map to include in the resulting 'ParseResult's
  LazyFixityMap ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  String ->
  m
    ( [GHC.Warn],
      Either (SrcSpan, String) [SourceSnippet]
    )
parseModule config@Config {..} fixityMap path rawInput = liftIO $ do
  -- It's important that 'setDefaultExts' is done before
  -- 'parsePragmasIntoDynFlags', because otherwise we might enable an
  -- extension that was explicitly disabled in the file.
  let baseFlags =
        GHC.setGeneralFlag'
          GHC.Opt_Haddock
          (setDefaultExts baseDynFlags)
      extraOpts = dynOptionToLocatedStr <$> cfgDynOptions
  (warnings, dynFlags) <-
    parsePragmasIntoDynFlags baseFlags extraOpts path rawInput >>= \case
      Right res -> pure res
      Left err ->
        let loc =
              mkSrcSpan
                (mkSrcLoc (GHC.mkFastString path) 1 1)
                (mkSrcLoc (GHC.mkFastString path) 1 1)
         in throwIO (OrmoluParsingFailed loc err)
  let cppEnabled = EnumSet.member Cpp (GHC.extensionFlags dynFlags)
  snippets <- runExceptT . forM (preprocess cppEnabled cfgRegion rawInput) $ \case
    Right region ->
      fmap ParsedSnippet . ExceptT $
        parseModuleSnippet (config $> region) fixityMap dynFlags path rawInput
    Left raw -> pure $ RawSnippet raw
  pure (warnings, snippets)

parseModuleSnippet ::
  MonadIO m =>
  Config RegionDeltas ->
  LazyFixityMap ->
  DynFlags ->
  FilePath ->
  String ->
  m (Either (SrcSpan, String) ParseResult)
parseModuleSnippet Config {..} fixityMap dynFlags path rawInput = liftIO $ do
  let (input, indent) = removeIndentation . linesInRegion cfgRegion $ rawInput
  let pStateErrors pstate =
        let errs = fmap pprError . bagToList $ GHC.getErrorMessages pstate
            fixupErrSpan = incSpanLine (regionPrefixLength cfgRegion)
         in case L.sortOn (Down . SeverityOrd . errMsgSeverity) errs of
              [] -> Nothing
              err : _ ->
                -- Show instance returns a short error message
                Just (fixupErrSpan (errMsgSpan err), show err)
      parser = case cfgSourceType of
        ModuleSource -> GHC.parseModule
        SignatureSource -> GHC.parseSignature
      r = case runParser parser dynFlags path input of
        GHC.PFailed pstate ->
          case pStateErrors pstate of
            Just err -> Left err
            Nothing -> error "PFailed does not have an error"
        GHC.POk pstate (L _ (normalizeModule -> hsModule)) ->
          case pStateErrors pstate of
            -- Some parse errors (pattern/arrow syntax in expr context)
            -- do not cause a parse error, but they are replaced with "_"
            -- by the parser and the modified AST is propagated to the
            -- later stages; but we fail in those cases.
            Just err -> Left err
            Nothing ->
              let (stackHeader, pragmas, comments) =
                    mkCommentStream input hsModule
               in Right
                    ParseResult
                      { prParsedSource = hsModule,
                        prSourceType = cfgSourceType,
                        prStackHeader = stackHeader,
                        prPragmas = pragmas,
                        prCommentStream = comments,
                        prExtensions = GHC.extensionFlags dynFlags,
                        prFixityOverrides = cfgFixityOverrides,
                        prFixityMap = fixityMap,
                        prIndent = indent
                      }
  return r

-- | Normalize a 'HsModule' by sorting its import\/export lists, dropping
-- blank comments, etc.
normalizeModule :: HsModule -> HsModule
normalizeModule hsmod =
  everywhere
    (mkT dropBlankTypeHaddocks)
    hsmod
      { hsmodImports =
          normalizeImports (hsmodImports hsmod),
        hsmodDecls =
          filter (not . isBlankDocD . unLoc) (hsmodDecls hsmod),
        hsmodHaddockModHeader =
          mfilter (not . isBlankDocString . unLoc) (hsmodHaddockModHeader hsmod),
        hsmodExports =
          (fmap . fmap) (filter (not . isBlankDocIE . unLoc)) (hsmodExports hsmod)
      }
  where
    isBlankDocString = all isSpace . unpackHDS
    isBlankDocD = \case
      DocD _ s -> isBlankDocString $ docDeclDoc s
      _ -> False
    isBlankDocIE = \case
      IEGroup _ _ s -> isBlankDocString s
      IEDoc _ s -> isBlankDocString s
      _ -> False

    dropBlankTypeHaddocks = \case
      L _ (HsDocTy _ ty (L _ ds)) :: LHsType GhcPs
        | isBlankDocString ds -> ty
      a -> a

-- | Enable all language extensions that we think should be enabled by
-- default for ease of use.
setDefaultExts :: DynFlags -> DynFlags
setDefaultExts flags = L.foldl' xopt_set (lang_set flags (Just Haskell2010)) autoExts
  where
    autoExts = allExts L.\\ manualExts
    allExts = [minBound .. maxBound]

-- | Extensions that are not enabled automatically and should be activated
-- by user.
manualExts :: [Extension]
manualExts =
  [ Arrows, -- steals proc
    Cpp, -- forbidden
    BangPatterns, -- makes certain patterns with ! fail
    PatternSynonyms, -- steals the pattern keyword
    RecursiveDo, -- steals the rec keyword
    StaticPointers, -- steals static keyword
    TransformListComp, -- steals the group keyword
    UnboxedTuples, -- breaks (#) lens operator
    MagicHash, -- screws {-# these things #-}
    AlternativeLayoutRule,
    AlternativeLayoutRuleTransitional,
    MonadComprehensions,
    UnboxedSums,
    UnicodeSyntax, -- gives special meanings to operators like (→)
    TemplateHaskell, -- changes how $foo is parsed
    TemplateHaskellQuotes, -- enables TH subset of quasi-quotes, this
    -- apparently interferes with QuasiQuotes in
    -- weird ways
    ImportQualifiedPost, -- affects how Ormolu renders imports, so the
    -- decision of enabling this style is left to the user
    NegativeLiterals, -- with this, `- 1` and `-1` have differing AST
    LexicalNegation, -- implies NegativeLiterals
    LinearTypes, -- steals the (%) type operator in some cases
    OverloadedRecordDot, -- f.g parses differently
    OverloadedRecordUpdate -- qualified fields are not supported
  ]

-- | Run a 'GHC.P' computation.
runParser ::
  -- | Computation to run
  GHC.P a ->
  -- | Dynamic flags
  GHC.DynFlags ->
  -- | Module path
  FilePath ->
  -- | Module contents
  String ->
  -- | Parse result
  GHC.ParseResult a
runParser parser flags filename input = GHC.unP parser parseState
  where
    location = mkRealSrcLoc (GHC.mkFastString filename) 1 1
    buffer = GHC.stringToStringBuffer input
    parseState = GHC.initParserState (opts flags) buffer location
    opts =
      GHC.mkParserOpts
        <$> GHC.warningFlags
        <*> GHC.extensionFlags
        <*> GHC.safeImportsOn
        <*> GHC.gopt GHC.Opt_Haddock
        <*> GHC.gopt GHC.Opt_KeepRawTokenStream
        <*> const True

-- | Wrap GHC's 'Severity' to add 'Ord' instance.
newtype SeverityOrd = SeverityOrd Severity

instance Eq SeverityOrd where
  s1 == s2 = compare s1 s2 == EQ

instance Ord SeverityOrd where
  compare (SeverityOrd s1) (SeverityOrd s2) =
    compare (f s1) (f s2)
    where
      f :: Severity -> Int
      f SevOutput = 1
      f SevFatal = 2
      f SevInteractive = 3
      f SevDump = 4
      f SevInfo = 5
      f SevWarning = 6
      f SevError = 7

----------------------------------------------------------------------------
-- Helpers taken from HLint

parsePragmasIntoDynFlags ::
  -- | Pre-set 'DynFlags'
  DynFlags ->
  -- | Extra options (provided by user)
  [Located String] ->
  -- | File name (only for source location annotations)
  FilePath ->
  -- | Input for parser
  String ->
  IO (Either String ([GHC.Warn], DynFlags))
parsePragmasIntoDynFlags flags extraOpts filepath str =
  catchErrors $ do
    let fileOpts = GHC.getOptions flags (GHC.stringToStringBuffer str) filepath
    (flags', leftovers, warnings) <-
      parseDynamicFilePragma flags (extraOpts <> fileOpts)
    case NE.nonEmpty leftovers of
      Nothing -> return ()
      Just unrecognizedOpts ->
        throwIO (OrmoluUnrecognizedOpts (unLoc <$> unrecognizedOpts))
    let flags'' = flags' `gopt_set` Opt_KeepRawTokenStream
    return $ Right (warnings, flags'')
  where
    catchErrors act =
      GHC.handleGhcException
        reportErr
        (GHC.handleSourceError reportErr act)
    reportErr e = return $ Left (show e)
