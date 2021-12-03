{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception
  ( Exception,
    IOException,
    catch,
    throwIO,
  )
import Control.Monad
  ( foldM,
    forM,
    when,
  )
import Data.Aeson (encodeFile)
import qualified Data.ByteString as ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.Hashable (Hashable)
import Data.List
  ( foldl',
    isPrefixOf,
    sortBy, isSuffixOf, sortOn, groupBy
  )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
  ( fromJust,
    fromMaybe, maybeToList
  )
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Format
  ( Format,
    Only (Only),
  )
import qualified Data.Text.Format as Format
import Data.Text.Format.Params (Params)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import GHC.Types.Fixity (FixityDirection (..))
import GHC.Utils.Monad (mapMaybeM)
import Options.Applicative
import Ormolu.Fixity
  ( FixityInfo (..),
    FixityMap,
    HoogleHackageInfo
      ( HoogleHackageInfo,
        hPackageToOps,
        hPackageToPopularity
      ),
  )
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath
  ( makeRelative,
    splitPath,
    (</>),
  )
import System.Posix.Files
  ( getFileStatus,
    isDirectory,
  )
import Text.HTML.TagSoup
  ( Tag (TagText),
    parseTags,
  )
import Text.HTML.TagSoup.Match
  ( tagCloseLit,
    tagOpenLit,
  )
import Text.Regex.Pcre2 (capture, regex)
import Distribution.PackageDescription.Parsec
import Data.HashSet (HashSet)
import Ormolu.Utils.Cabal (getExtensionAndDepsMap)
import Data.Foldable (forM_)
import Data.Ord (Down(Down))
import Debug.Trace (trace)

data PopularityConfig = PopularityConfig {
  revDepBonus :: Float,
  revDepDlCountRatio :: Float,
  ownDlCountRatio :: Float,
  transitiveRatio :: Float
}

defaultPopularityConfig :: PopularityConfig
defaultPopularityConfig = PopularityConfig
  {
    revDepBonus = 1.0,
    revDepDlCountRatio = 1.0,
    ownDlCountRatio = 1.0,
    transitiveRatio = 0.9
  }

defaultOutputPath :: FilePath
defaultOutputPath = "extract-hoogle-hackage-info/hoogle-hackage-info.json"

baseInitialFixityMap :: HashMap String [FixityInfo]
baseInitialFixityMap =
  HashMap.singleton
    ":"
    [FixityInfo {fixDir = Just InfixR, fixMinPrec = 5, fixMaxPrec = 5}]

unspecifiedFixityInfo :: FixityInfo
unspecifiedFixityInfo = FixityInfo (Just InfixL) 9 9

initialHoogleState :: HoogleState
initialHoogleState =
  HoogleState
    { hosPackageToOps = HashMap.empty,
      hosProcessedFiles = 0
    }

initialHackageState :: HackageState
initialHackageState =
  HackageState
    { hasPackageToRevDeps = HashMap.empty,
      hasProcessedFiles = 0
    }

newtype SimpleException = SimpleException Text deriving (Eq, Show)

instance Exception SimpleException

data HoogleState = HoogleState
  { hosPackageToOps :: HashMap String (HashMap String [FixityInfo]),
    hosProcessedFiles :: Int
  }
  deriving (Eq, Show)

data HackageState = HackageState
  { hasPackageToRevDeps :: HashMap String (HashSet String),
    hasProcessedFiles :: Int
  }

data Config = Config
  { cfgHoogleDatabasePath :: FilePath,
    cfgHackageDlCountsPath :: FilePath,
    cfgHackageDatabasePath :: FilePath,
    cfgOutputPath :: FilePath,
    cfgDebugLimit :: Maybe Int
  }
  deriving (Eq, Show)

-- | format using the Strict variant of Data.Text
format :: forall ps. Params ps => Format -> ps -> Text
format f p = TL.toStrict $ Format.format f p

-- | Put a formatted string
putFmtLn :: forall ps. Params ps => Format -> ps -> IO ()
putFmtLn f p = TIO.putStrLn $ format f p

showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> a
readT = read . T.unpack

indentLines :: [Text] -> [Text]
indentLines = fmap ("  " <>)

-- | Recursively list all files inside directory
walkDir ::
  -- | Path to the root directory
  FilePath ->
  -- | Whether we should exclude a file of the result list based on its path
  (FilePath -> Bool) ->
  IO [FilePath]
walkDir top exclude = do
  ds <- listDirectory top
  paths <- forM ds $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then walkDir path exclude
      else return $ filter (not . exclude) [path]
  return (concat paths)

getPackageName ::
  -- | Root path
  FilePath ->
  -- | Current file path
  FilePath ->
  IO Text
getPackageName rootPath filePath = do
  when (not (rootPath `isPrefixOf` filePath)) $
    throwIO . SimpleException $
      format
        "{} do not start with {}"
        (T.pack filePath, T.pack rootPath)
  let packageName =
        stripSuffix' "/" $
          T.pack . head . splitPath $
            makeRelative rootPath filePath
      stripSuffix' suffix txt = fromMaybe txt $ T.stripSuffix suffix txt
  when (T.null packageName) $
    throwIO . SimpleException $
      format
        "Extracted package name is empty for {} (base path = {})"
        (T.pack filePath, T.pack rootPath)
  return packageName

-- | Try to read the specified file using utf-8 encoding first, and latin1 otherwise
readFileUtf8Latin1 :: FilePath -> IO Text
readFileUtf8Latin1 filePath = catch @IOException (TIO.readFile filePath) $ \e -> do
  putFmtLn
    "Unable to read {} with UTF-8, trying latin1: {}"
    (filePath, show e)
  decodeLatin1 <$> ByteString.readFile filePath

firstMiddleLast :: [a] -> Maybe (a, [a], a)
firstMiddleLast string = case string of
  x1 : x2 : xs -> Just (x1, init (x2 : xs), last (x2 : xs))
  _ -> Nothing

declToNormName :: String -> String
declToNormName declOpName = case firstMiddleLast declOpName of
  Just ('(', middle, ')') -> middle
  _ -> declOpName

infixToNormName :: String -> String
infixToNormName infixOpName = case firstMiddleLast infixOpName of
  Just ('`', middle, '`') -> middle
  _ -> infixOpName

onSymbolDecl :: Text -> HoogleState -> Text -> HoogleState
onSymbolDecl packageName state@HoogleState {..} declOpName =
  let hosPackageToOps' = case HashMap.lookup packageName' hosPackageToOps of
        Nothing
          | packageName' == "base" ->
              HashMap.insert
                packageName'
                (HashMap.insert normOpName [] baseInitialFixityMap)
                hosPackageToOps
        Nothing ->
          HashMap.insert
            packageName'
            (HashMap.singleton normOpName [])
            hosPackageToOps
        Just packageFixityMap -> case HashMap.lookup normOpName packageFixityMap of
          Nothing ->
            HashMap.insert
              packageName'
              (HashMap.insert normOpName [] packageFixityMap)
              hosPackageToOps
          Just _ -> hosPackageToOps
      normOpName = declToNormName . T.unpack $ declOpName
      packageName' = T.unpack packageName
   in state {hosPackageToOps = hosPackageToOps'}

onFixityDecl :: Text -> HoogleState -> (Text, Text, Text) -> HoogleState
onFixityDecl packageName state@HoogleState {..} (rawFixDir, rawFixPrec, infixOpName) =
  let hosPackageToOps' = case HashMap.lookup packageName' hosPackageToOps of
        Nothing
          | packageName' == "base" ->
              HashMap.insert
                  packageName'
                  (HashMap.insert normOpName [fixDecl] baseInitialFixityMap)
                  hosPackageToOps
        Nothing ->
          HashMap.insert
              packageName'
              (HashMap.singleton normOpName [fixDecl])
              hosPackageToOps
        Just packageFixityMap -> case fromMaybe [] $ HashMap.lookup normOpName packageFixityMap of
          [] ->
            HashMap.insert
                packageName'
                (HashMap.insert normOpName [fixDecl] packageFixityMap)
                hosPackageToOps
          fixDecls
            | fixDecl `elem` fixDecls ->
                hosPackageToOps
          fixDecls ->
            HashMap.insert
                packageName'
                (HashMap.insert normOpName (fixDecl : fixDecls) packageFixityMap)
                hosPackageToOps
      packageName' = T.unpack packageName
      normOpName = infixToNormName $ T.unpack infixOpName
      fixDecl =
        let fixPrec = readT rawFixPrec
         in FixityInfo
              { fixDir = Just (readFixDir . T.unpack $ rawFixDir),
                fixMinPrec = fixPrec,
                fixMaxPrec = fixPrec
              }
      readFixDir = \case
        "infix" -> InfixN
        "infixr" -> InfixR
        "infixl" -> InfixL
        other -> error $ "unexpected fixity direction: " ++ other
   in state {hosPackageToOps = hosPackageToOps'}

finalizePackageToOps ::
  HashMap String (HashMap String [FixityInfo]) ->
  (HashMap String FixityMap, [((String, String), [FixityInfo])])
finalizePackageToOps hashmap =
  ( HashMap.map (HashMap.map finalize) hashmap,
    concat $ injectFst <$> (HashMap.toList . HashMap.map (HashMap.toList . HashMap.filter hasConflict) $ hashmap)
  )
  where
    finalize = \case
      [] -> unspecifiedFixityInfo
      fs -> sconcat . NE.fromList $ fs
    hasConflict = (> 1) . length
    injectFst (packageName, opFixs) = fmap (\(opName, fixs) -> ((packageName, opName), fixs)) opFixs

extractFixitiesFromFile ::
  -- | Hoogle database path
  FilePath ->
  HoogleState ->
  -- | Current file path
  FilePath ->
  IO HoogleState
extractFixitiesFromFile hoogleDatabasePath state@HoogleState {hosProcessedFiles} filePath = do
  fileContent <- readFileUtf8Latin1 filePath
  packageName <- getPackageName hoogleDatabasePath filePath
  let state' =
        foldl' @[]
          (onSymbolDecl packageName)
          state
          (fromSymbolDecl <$> symbolDecls fileContent)
      state'' =
        foldl' @[]
          (onFixityDecl packageName)
          state'
          (fromFixityDecl <$> fixityDecls fileContent)
      fromSymbolDecl match = capture @"declOpName" match
      fromFixityDecl match =
        ( capture @"fixDir" match,
          capture @"fixPrec" match,
          capture @"infixOpName" match
        )
      symbolDecls = [regex|(?m)^\s*?(?<declOpName>\([^)]+?\))\s*?::.*$|]
      fixityDecls = [regex|(?m)^\s*?(?<fixDir>infix[rl]?)\s+?(?<fixPrec>[0-9])\s+?(?<infixOpName>[^\s]+)\s*$|]
  return state'' {hosProcessedFiles = hosProcessedFiles + 1}

extractHoogleInfo :: FilePath -> IO (HashMap String FixityMap)
extractHoogleInfo hoogleDatabasePath = do
  hoogleFiles <- walkDir hoogleDatabasePath (const False)
  HoogleState {..} <-
    foldM
      (extractFixitiesFromFile hoogleDatabasePath)
      initialHoogleState
      hoogleFiles
  putFmtLn
    "{} hoogle files processed!"
    (Only hosProcessedFiles)
  let (packageToOps, conflicts) = finalizePackageToOps hosPackageToOps
  putFmtLn
    "Found {} operator declarations across {} packages for a total of {} distinct operators"
    (getCounts packageToOps)
  when (not (null conflicts)) $
    displayConflicts conflicts
  return packageToOps

displayConflicts :: [((String, String), [FixityInfo])] -> IO ()
displayConflicts conflicts = do
  putFmtLn
    "Found {} conflicting declarations within packages themselves:"
    (Only $ length conflicts)
  TIO.putStrLn $ T.intercalate "\n" conflictLines'
  where
    conflictLines' = concat $ conflictLines <$> sortedConflicts
    sortedConflicts =
      sortBy
        (\(packageOp1, _) (packageOp2, _) -> compare packageOp1 packageOp2)
        conflicts
    conflictLines ((packageName, opName), fixities) =
      format
        "{} in {}:"
        (packageName, opName)
        : indentLines (showT <$> fixities)

limitMapWith ::
  (Eq k, Hashable k) =>
  (v -> v') ->
  Int ->
  HashMap k v ->
  HashMap k v'
limitMapWith f n hashmap =
  HashMap.fromList $
    (\k -> (k, f . fromJust $ HashMap.lookup k hashmap)) <$> limitedKeys
  where
    limitedKeys = take n $ HashMap.keys hashmap

getCounts :: HashMap String FixityMap -> (Int, Int, Int)
getCounts packageToOps = (declCount, packagesCount, distinctOpCount)
  where
    packagesCount = HashMap.size packageToOps
    declCount = sum $ HashMap.size <$> fixityMaps
    distinctOpCount =
      HashSet.size . HashSet.fromList . concat $
        HashMap.keys <$> fixityMaps
    fixityMaps = HashMap.elems packageToOps

getDependenciesFromCabalFile ::
  FilePath ->
  IO [String]
getDependenciesFromCabalFile cabalFileAsGiven = do
  cabalFile <- makeAbsolute cabalFileAsGiven
  cabalFileBs <- ByteString.readFile cabalFile
  genericPackageDescription <-
    case parseGenericPackageDescriptionMaybe cabalFileBs of
      Just gpd -> pure gpd
      Nothing -> throwIO . SimpleException $ format "Unable to parse cabal file: {}" (Only cabalFile)
  let extDepMap = getExtensionAndDepsMap cabalFile genericPackageDescription
  return . concatMap snd . Map.elems $ extDepMap

extractReverseDependenciesFromFile ::
  -- | Hackage database path
  FilePath ->
  HackageState ->
  -- | Current file path
  FilePath ->
  IO HackageState
extractReverseDependenciesFromFile hackageDatabasePath HackageState{..} cabalFile = do
  deps <- getDependenciesFromCabalFile cabalFile
  packageName <- T.unpack <$> getPackageName hackageDatabasePath cabalFile
  let hasPackageToRevDeps' = foldl' (insertRevDep packageName) hasPackageToRevDeps deps
  return HackageState
    { hasPackageToRevDeps = hasPackageToRevDeps',
      hasProcessedFiles = hasProcessedFiles + 1
    }
  where
    insertRevDep packageName packageToRevDeps depName =
      HashMap.insertWith HashSet.union depName (HashSet.singleton packageName) packageToRevDeps

extractHackageReverseDependencies :: FilePath -> IO (HashMap String (HashSet String))
extractHackageReverseDependencies hackageDatabasePath = do
  cabalFiles <- walkDir hackageDatabasePath (not . isSuffixOf ".cabal")
  HackageState {..} <-
    foldM
      (extractReverseDependenciesFromFile hackageDatabasePath)
      initialHackageState
      cabalFiles
  putFmtLn
    "{} cabal files processed!"
    (Only hasProcessedFiles)
  return hasPackageToRevDeps

extractHackageDlCounts :: FilePath -> IO (HashMap String Int)
extractHackageDlCounts filePath = do
  content <- TIO.readFile filePath
  let soup = filterBlankTags $ parseTags content
      tableBody =
        tail $
          takeWhile (not . tagCloseLit "tbody") $
            dropWhile (not . tagOpenLit "tbody" (const True)) soup
      processRow tags = case extractText <$> groupOn "td" tags of
        rawName : rawDlCount : _ -> return $ Just (name, dlCount)
          where
            name = T.unpack . T.strip . head $ T.split (== ' ') rawName
            dlCount = readT $ T.strip rawDlCount :: Int
        _ -> do
          putFmtLn
            "Invalid line: {}"
            (Only $ T.intercalate " " $ showT <$> tags)
          return Nothing
      extractText tags = T.intercalate "" $ extractText' <$> tags
      extractText' = \case
        TagText t -> t
        _ -> ""
      groupOn _ [] = []
      groupOn selector (_ : ts) =
        let (tags, remTags) = break (tagOpenLit selector (const True)) ts
         in init tags : groupOn selector remTags
      filterBlankTags =
        filter
          ( \case
              TagText t | isBlank t -> False
              _ -> True
          )
      isBlank t = null $ dropWhile (`elem` [' ', '\t', '\n']) (T.unpack t)
  result <- HashMap.fromList <$> mapMaybeM processRow (groupOn "tr" tableBody)
  putFmtLn
    "Found 30 days DL counts for {} packages"
    (Only $ HashMap.size result)
  return result

pre12 :: (String -> Int) -> HashMap String (HashSet String) -> HashMap (Int, Int) Int
pre12 nameToIdx = HashMap.fromList . concatMap makePairs . HashMap.toList where
  makePairs (packageName, revDepsSet) =
    fmap (\revDepName -> ((nameToIdx packageName, nameToIdx revDepName), 1)) (HashSet.toList revDepsSet)
post12 :: (Int -> String) -> HashMap (Int, Int) Int -> HashMap String [(String, Int)]
post12 idxToName = HashMap.fromListWith (<>) . fmap unmakePairs . HashMap.toList where
  unmakePairs ((packageIdx, revDepIdx), depth) = (idxToName packageIdx, [(idxToName revDepIdx, depth)])

floydWarshallSparse1 :: (Eq k, Hashable k, Ord v, Num v) => HashMap (k, k) v -> HashMap (k, k) v
floydWarshallSparse1 baseGraph = trace ("  Found " ++ show (length vertices) ++ " vertices") (whileNeq (1 :: Int) floydWarshallStep baseGraph (trace "  Round 1 of Floyd Warshall" (floydWarshallStep baseGraph)))
  where
      vertices = HashSet.toList . HashSet.fromList . concatMap (\(a, b) -> [a, b]) . HashMap.keys $ baseGraph
      whileNeq n f a b
        | a == b = a
        | otherwise = whileNeq (n + 1) f b (trace ("  Round " ++ show (n + 1) ++ " of Floyd Warshall") (f b))
      floydWarshallStep graph = trace ("  " ++ show (HashMap.size graph) ++ " edges and " ++ show (length newEdges) ++ " potential new edges") (insertsWith min newEdges graph) where
        newEdges = [ ((vI, vJ), lengthIK + lengthKJ) |
                     ((vI, vK), lengthIK) <- HashMap.toList graph,
                     ((vK', vJ), lengthKJ) <- HashMap.toList graph,
                     vK == vK'
                   ]
        insertsWith f = flip (foldl' (\hm (k, v) -> HashMap.insertWith f k v hm))

floydWarshallSparse2 :: (Eq k, Hashable k, Ord v, Num v) => HashMap (k, k) v -> HashMap (k, k) v
floydWarshallSparse2 baseGraph = trace ("  Found " ++ show (length vertices) ++ " vertices") (whileNeq (1 :: Int) floydWarshallStep baseGraph (trace "  Round 1 of Floyd Warshall" (floydWarshallStep baseGraph)))
  where
      vertices = HashSet.toList . HashSet.fromList . concatMap (\(a, b) -> [a, b]) . HashMap.keys $ baseGraph
      whileNeq n f a b
        | a == b = a
        | otherwise = whileNeq (n + 1) f b (trace ("  Round " ++ show (n + 1) ++ " of Floyd Warshall") (f b))
      floydWarshallStep graph = trace ("  " ++ show (HashMap.size graph) ++ " edges and " ++ show (length newEdges) ++ " potential new edges") (insertsWith min newEdges graph) where
        newEdges = [ ((vI, vJ), lengthIK + lengthKJ) |
                     ((vI, vK), lengthIK) <- HashMap.toList graph,
                     vJ <- vertices,
                     lengthKJ <- maybeToList $ HashMap.lookup (vK, vJ) graph
                   ]
        insertsWith f = flip (foldl' (\hm (k, v) -> HashMap.insertWith f k v hm))

pre3 :: (String -> Int) -> HashMap String (HashSet String) -> HashMap Int (HashMap Int Int)
pre3 nameToIdx = HashMap.fromList . fmap makePairs . HashMap.toList where
  makePairs (packageName, revDepsSet) =
    (nameToIdx packageName, HashMap.fromList . fmap (\revDepName -> (nameToIdx revDepName, 1)) . HashSet.toList $ revDepsSet)
post3 :: (Int -> String) -> HashMap Int (HashMap Int Int) -> HashMap String [(String, Int)]
post3 idxToName = HashMap.fromList . fmap unmakePairs . HashMap.toList where
  unmakePairs (packageIdx, neighbours) = (idxToName packageIdx, fmap (\(revDepIdx, depth) -> (idxToName revDepIdx, depth)) . HashMap.toList $ neighbours)

floydWarshallSparse3 :: (Eq k, Hashable k, Ord v, Num v) => HashMap k (HashMap k v) -> HashMap k (HashMap k v)
floydWarshallSparse3 baseGraph = trace ("  Found " ++ show (length vertices) ++ " vertices") (whileNeq (1 :: Int) floydWarshallStep baseGraph (trace "  Round 1 of Floyd Warshall" (floydWarshallStep baseGraph)))
  where
      vertices = HashSet.toList . HashSet.fromList $
        HashMap.keys baseGraph
          <> concat (HashMap.map HashMap.keys baseGraph)
      whileNeq n f a b
        | a == b = a
        | otherwise = whileNeq (n + 1) f b (trace ("  Round " ++ show (n + 1) ++ " of Floyd Warshall") (f b))
      floydWarshallStep graph = trace ("  " ++ show (foldl' (\t shm -> t + HashMap.size shm) 0 graph) ++ " edges and " ++ show (length newEdges) ++ " potential new edges") (insertsWith min newEdges graph) where
        newEdges = [ ((vI, vJ), lengthIK + lengthKJ) |
                     (vI, neighbours) <- HashMap.toList graph,
                     (vK, lengthIK) <- HashMap.toList neighbours,
                     (vJ, lengthKJ) <- maybe [] HashMap.toList $ HashMap.lookup vK graph
                   ]
        insertsWith f = flip (foldl' (\hm ((k1, k2), v) -> HashMap.insertWith (HashMap.unionWith f) k1 (HashMap.singleton k2 v) hm))

pre4 :: (String -> Int) -> HashMap String (HashSet String) -> HashMap Int [(Int, Int)]
pre4 nameToIdx = HashMap.fromList . fmap makePairs . HashMap.toList where
  makePairs (packageName, revDepsSet) =
    (nameToIdx packageName, sortOn fst . fmap (\revDepName -> (nameToIdx revDepName, 1)) . HashSet.toList $ revDepsSet)
post4 :: (Int -> String) -> HashMap Int [(Int, Int)] -> HashMap String [(String, Int)]
post4 idxToName = HashMap.fromList . fmap unmakePairs . HashMap.toList where
  unmakePairs (packageIdx, neighbours) = (idxToName packageIdx, (\(revDepIdx, depth) -> (idxToName revDepIdx, depth)) <$> neighbours)

floydWarshallSparse4 :: (Ord k, Hashable k, Ord v, Num v) => HashMap k [(k, v)] -> HashMap k [(k, v)]
floydWarshallSparse4 baseGraph = trace ("  Found " ++ show (length vertices) ++ " vertices") (whileNeq (1 :: Int) floydWarshallStep baseGraph (trace "  Round 1 of Floyd Warshall" (floydWarshallStep baseGraph)))
  where
      vertices = HashSet.toList . HashSet.fromList $
        HashMap.keys baseGraph
          <> concat (HashMap.map (fmap fst) baseGraph)
      whileNeq n f a b
        | a == b = a
        | otherwise = whileNeq (n + 1) f b (trace ("  Round " ++ show (n + 1) ++ " of Floyd Warshall") (f b))
      floydWarshallStep graph = trace ("  " ++ show (foldl' (\t pairs -> t + length pairs) 0 graph) ++ " edges and " ++ show (length newEdges) ++ " potential new edges") (insertsWithMin (groupEdges newEdges) graph) where
        newEdges = [ (vI, (vJ, lengthIK + lengthKJ)) |
                     (vI, neighbours) <- HashMap.toList graph,
                     (vK, lengthIK) <- neighbours,
                     (vJ, lengthKJ) <- fromMaybe [] $ HashMap.lookup vK graph
                   ]
        groupEdges edges = groupBy (\(src1, _) (src2, _) -> src1 == src2) edges
        insertsWithMin groupedPairs graph' = foldl' insertPairs graph' groupedPairs where
          insertPairs hm pairs = let (vI, _):_ = pairs in
            HashMap.insertWith mergeSortedFst vI (sortOn fst (snd <$> pairs)) hm
        mergeSortedFst [] xs2 = xs2
        mergeSortedFst xs1 [] = xs1
        mergeSortedFst r1@(t1@(v1, l1):xs1) r2@(t2@(v2, l2):xs2)
          | v1 < v2 = t1 : mergeSortedFst xs1 r2
          | v1 > v2 = t2 : mergeSortedFst r1 xs2
          | otherwise = (v1, min l1 l2) : mergeSortedFst xs1 xs2

-- strategies
-- 1. use list of pairs in strat 3
-- 2. use unboxed vector of pairs in strat 3
-- 3. use non-sparse matrix
-- 4. use topological order

buildTransRevDeps ::
  Int -> HashMap String (HashSet String) -> HashMap String [(String, Int)]
buildTransRevDeps strategy hm = case strategy of
  1 -> post12 idxToName . floydWarshallSparse1 . pre12 nameToIdx $ hm
  2 -> post12 idxToName . floydWarshallSparse2 . pre12 nameToIdx $ hm
  3 -> post3 idxToName . floydWarshallSparse3 . pre3 nameToIdx $ hm
  _ -> post4 idxToName . floydWarshallSparse4 . pre4 nameToIdx $ hm
  where
    allKeys = HashSet.toList . HashSet.unions $ (HashMap.keysSet hm : HashMap.elems hm)
    idxToNameMap = HashMap.fromList $ zip [0..] allKeys
    nameToIdxMap = HashMap.fromList $ zip allKeys [0..]
    idxToName = fromJust . flip HashMap.lookup idxToNameMap
    nameToIdx = fromJust . flip HashMap.lookup nameToIdxMap

buildPackageToPop :: PopularityConfig -> HashMap String Int -> HashMap String [(String, Int)] -> HashMap String Float
buildPackageToPop PopularityConfig{..} packageToDlCount packageToTransRevDeps =
    HashMap.unionWith (+)
        (HashMap.map (\c -> fromIntegral c * ownDlCountRatio) packageToDlCount)
        (HashMap.map sumDlCounts packageToTransRevDeps)
    where
      sumDlCounts revDeps = sum $ (\(revDepName, depth) -> (revDepBonus + revDepDlCountRatio * (fromIntegral . fromMaybe 0 $ HashMap.lookup revDepName packageToDlCount)) * transitiveRatio ** (fromIntegral depth - 1)) <$> revDeps

extractHackageInfo :: FilePath -> FilePath -> PopularityConfig -> IO (HashMap String Float)
extractHackageInfo hackageDlCountsPath hackageDatabasePath popularityConfig = do
  packageToDlCount <- extractHackageDlCounts hackageDlCountsPath
  packageToRevDeps <- extractHackageReverseDependencies hackageDatabasePath
  let packageToTransRevDeps = buildTransRevDeps 2 packageToRevDeps
  return $ buildPackageToPop popularityConfig packageToDlCount packageToTransRevDeps

displayTopPackages :: HashMap String Float -> IO ()
displayTopPackages packageToPop = do
  let packageToPop' = take 100 . zip ([1..] :: [Int]) . sortOn (Down . snd) . HashMap.toList $ packageToPop
  forM_ packageToPop' $ \(i, (packageName, score)) -> putFmtLn "{}. {} (score = {})" (Format.left 4 ' ' i, packageName, score)

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser) fullDesc
  where
    configParser :: Parser Config
    configParser =
      Config
        <$> (strArgument . mconcat)
          [ metavar "HOOGLE_DATABASE_PATH",
            help "Download: mkdir -p hoogle-database && curl https://hackage.haskell.org/packages/hoogle.tar.gz | tar -xz -C hoogle-database"
          ]
        <*> (strArgument . mconcat)
          [ metavar "HACKAGE_DL_COUNTS_PATH",
            help "Donwload: curl https://hackage.haskell.org/packages/browse -o hackage-database.html"
          ]
        <*> (strArgument . mconcat)
          [ metavar "HACKAGE_DATABASE_PATH",
            help "Download: mkdir -p hackage-database && curl https://hackage.haskell.org/packages/index.tar.gz | tar -xz -C hackage-database"
          ]
        <*> (strOption . mconcat)
          [ short 'o',
            long "output-path",
            metavar "OUTPUT_PATH",
            value defaultOutputPath
          ]
        <*> (option (Just <$> auto) . mconcat)
          [ short 'd',
            long "debug-limit",
            metavar "N",
            value Nothing
          ]

-- | Entry point of the program.
main :: IO ()
main = do
  Config {..} <- execParser configParserInfo
  packageToOps <- extractHoogleInfo cfgHoogleDatabasePath
  packageToPop <- extractHackageInfo cfgHackageDlCountsPath cfgHackageDatabasePath defaultPopularityConfig
  let (packageToOps', packageToPop') = case cfgDebugLimit of
        Nothing -> (packageToOps, packageToPop)
        Just n ->
          ( limitMapWith (limitMapWith id n) n packageToOps,
            limitMapWith id n packageToPop
          )
  encodeFile cfgOutputPath $
    HoogleHackageInfo
      { hPackageToOps = packageToOps',
        hPackageToPopularity = packageToPop'
      }
  displayTopPackages packageToPop'
