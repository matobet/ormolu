{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
-- See https://github.com/haskell/haskell-language-server/issues/1841#issuecomment-843378909 if you encounter an issue with HLS
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ormolu.Fixity
  ( FixityInfo (..),
    FixityConfig (..),
    PackageConfig (..),
    ConflictStrategy (..),
    FixityMap,
    HoogleHackageInfo (..),
    defaultFixityInfo,
    defaultFixityMap,
    defaultFixityConfig,
    defaultConflictStrategy,
    validateFixityConfig,
    buildFixityMap,
  )
where

import Data.Aeson (FromJSON, ToJSON, decodeStrict)
import qualified Data.ByteString
import Data.FileEmbed (embedFile)
import Data.Foldable (Foldable (foldMap'), foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (sconcat)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Types.Fixity (FixityDirection (..))

deriving instance Generic FixityDirection

deriving instance Show FixityDirection

instance Hashable FixityDirection

instance FromJSON FixityDirection

instance ToJSON FixityDirection

type FixityMap = HashMap String FixityInfo

data HoogleHackageInfo = HoogleHackageInfo
  { hPackageToOps :: HashMap String FixityMap,
    hPackageToPopularity :: HashMap String Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON HoogleHackageInfo

instance ToJSON HoogleHackageInfo

hoogleHackageInfoFile :: Data.ByteString.ByteString
hoogleHackageInfoFile = $(embedFile "extract-hoogle-hackage-info/hoogle-hackage-info.json")

packageToOps :: HashMap String FixityMap
packageToPopularity :: HashMap String Int
HoogleHackageInfo {hPackageToOps = packageToOps, hPackageToPopularity = packageToPopularity} = fromJust . decodeStrict $ hoogleHackageInfoFile

-- | Gives fixity information (direction and precedence level) about an infix operator, but takes the uncertainty that can arise from conflicting definitions into account.
data FixityInfo = FixityInfo
  { -- | Fixity direction (InfixL, InfixR, or InfixN (not associative)), if it is known
    fixDir :: Maybe FixityDirection,
    -- | Minimum precedence level found in the (maybe conflicting) definitions for the operator (inclusive)
    fixMinPrec :: Int,
    -- | Maximum precedence level found in the (maybe conflicting) definitions for the operator (inclusive)
    fixMaxPrec :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON FixityInfo

instance ToJSON FixityInfo

-- | Corresponds to the lowest level of information we can get about an operator (no information for fixity direction, and a precedence between 0 and 9).
defaultFixityInfo :: FixityInfo
defaultFixityInfo =
  FixityInfo
    { fixDir = Nothing,
      fixMinPrec = 0,
      fixMaxPrec = 9
    }

-- | Gives the ability to merge two (maybe conflicting) definitions for an operator, keeping the higher level of compatible information from both.
instance Semigroup FixityInfo where
  FixityInfo {fixDir = dir1, fixMinPrec = min1, fixMaxPrec = max1} <> FixityInfo {fixDir = dir2, fixMinPrec = min2, fixMaxPrec = max2} =
    FixityInfo {fixDir = dir', fixMinPrec = min min1 min2, fixMaxPrec = max max1 max2}
    where
      dir' = case (dir1, dir2) of
        (Just a, Just b) | a == b -> Just a
        _ -> Nothing

instance Hashable FixityInfo

data FixityConfig = FixityConfig
  { fcOpsManualConfig :: FixityMap,
    -- ordering matters for this field
    fcPackagesManualConfig :: [(String, PackageConfig)],
    fcDetectedCabalDependencies :: [String],
    -- Nothing means do not use hoogle
    fcUseHoogleForUnspecifiedPackages :: Maybe ConflictStrategy
  }
  deriving (Eq, Show, Generic)

defaultConflictStrategy :: ConflictStrategy
defaultConflictStrategy = UseThreshold 0.9

defaultFixityConfig :: FixityConfig
defaultFixityConfig =
  FixityConfig
    { fcOpsManualConfig = HashMap.empty,
      fcPackagesManualConfig = [],
      fcDetectedCabalDependencies = [],
      fcUseHoogleForUnspecifiedPackages = Just defaultConflictStrategy
    }

defaultFixityMap :: FixityMap
defaultFixityMap = buildFixityMap defaultFixityConfig

data PackageConfig
  = AllExcept [String]
  | NoneExcept [String]
  deriving (Eq, Show, Generic)

data ConflictStrategy
  = KeepBest
  | MergeAll
  | UseThreshold Float
  deriving (Eq, Show, Generic)

-- | Right = validated/normalized fixity config, Left = list of errors
validateFixityConfig :: FixityConfig -> Either [String] FixityConfig
validateFixityConfig cfg@FixityConfig {..} =
  if not (null errors)
    then Left errors
    else Right cfg
  where
    mWhen cond v = if cond then v else mempty
    errors =
      foldMap' checkManualOpCfg (HashMap.toList fcOpsManualConfig)
        <> foldMap' checkManualPackageCfg fcPackagesManualConfig
        <> checkConflictStrategy fcUseHoogleForUnspecifiedPackages
    checkManualOpCfg (opName, fixInfo@FixityInfo {fixMinPrec, fixMaxPrec}) =
      mWhen
        (fixMinPrec > fixMaxPrec)
        ["Min precedence level > max precedence level for operator " ++ opName ++ " in the manual map: " ++ show fixInfo]
        <> mWhen
          (fixMinPrec < 0 || fixMinPrec > 9)
          ["Min precedence level out of bounds [|0, 9|] for operator " ++ opName ++ " in the manual map: " ++ show fixInfo]
        <> mWhen
          (fixMaxPrec < 0 || fixMaxPrec > 9)
          ["Max precedence level out of bounds [|0, 9|] for operator " ++ opName ++ " in the manual map: " ++ show fixInfo]
    checkManualPackageCfg (packageName, packageConfig) = case HashMap.lookup packageName packageToOps of
      Just packageOps ->
        foldMap' checkListedOp listedOps
        where
          listedOps = case packageConfig of
            AllExcept ops -> ops
            NoneExcept ops -> ops
          checkListedOp listedOp =
            mWhen
              (not (listedOp `HashMap.member` packageOps))
              ["Operator " ++ listedOp ++ " in the include/exclude list for package " ++ packageName ++ " cannot be found"]
      Nothing -> ["Package " ++ packageName ++ " cannot be found"]
    checkConflictStrategy = \case
      Just (UseThreshold t) | t < 0.0 || t > 1.0 -> ["Threshold for conflict resolution not in range [0,1]: " ++ show t]
      _ -> []

buildFixityMap :: FixityConfig -> FixityMap
buildFixityMap FixityConfig {..} =
  addOverride fcOpsManualConfig postManualConfigFixityMap
  where
    postManualConfigFixityMap = foldr addOverride postCabalFixityMap (snd . buildPackageFixityMap <$> fcPackagesManualConfig')
    fcPackagesManualConfig' =
      if any ((== "base") . fst) fcPackagesManualConfig
        then fcPackagesManualConfig
        else ("base", AllExcept []) : fcPackagesManualConfig
    postCabalFixityMap = addOverride cabalFixityMap hoogleFixityMap
    cabalFixityMap = mergeFixityMaps KeepBest (buildPackageFixityMap <$> allOpsFromEach cabalDependenciesNotManuallyConfigured)
    cabalDependenciesNotManuallyConfigured = Set.toList $ Set.fromList fcDetectedCabalDependencies `Set.difference` Set.fromList (fst <$> fcPackagesManualConfig')
    hoogleFixityMap = case fcUseHoogleForUnspecifiedPackages of
      Nothing -> HashMap.empty
      Just conflictStrategy -> mergeFixityMaps conflictStrategy (buildPackageFixityMap <$> allOpsFromEach unspecifiedPackages)
    specifiedPackages = (fst <$> fcPackagesManualConfig') ++ cabalDependenciesNotManuallyConfigured
    unspecifiedPackages = Set.toList $ Set.fromList (HashMap.keys packageToOps) `Set.difference` Set.fromList specifiedPackages
    allOpsFromEach xs = zip xs (repeat $ AllExcept [])
    addOverride = HashMap.union -- HashMap.union is left biaised

mergeFixityMaps :: ConflictStrategy -> [(String, FixityMap)] -> FixityMap
mergeFixityMaps strategy packagesMaps =
  HashMap.map
    ( ( case strategy of
          KeepBest -> keepBest
          MergeAll -> mergeAll
          UseThreshold t -> useThreshold t
      )
        . NE.fromList
        . HashMap.toList
    )
    scoredMap
  where
    keepBest :: NonEmpty (FixityInfo, Int) -> FixityInfo
    keepBest = sconcat . fmap fst . maxWith snd
    mergeAll :: NonEmpty (FixityInfo, Int) -> FixityInfo
    mergeAll = sconcat . fmap fst
    useThreshold :: Float -> NonEmpty (FixityInfo, Int) -> FixityInfo
    useThreshold t fixScores =
      if (fromIntegral maxScore :: Float) / (fromIntegral sumScores :: Float) >= t
        then keepBest fixScores
        else mergeAll fixScores
      where
        maxs = maxWith snd fixScores
        maxScore = snd $ NE.head maxs
        sumScores = foldl' (+) 0 (snd <$> fixScores)
    maxWith :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
    maxWith f xs = snd $ foldl' comp (f h, h :| []) t
      where
        h :| t = xs
        comp (fMax, maxs) x =
          let fX = f x
           in if
                  | fMax < fX -> (fX, x :| [])
                  | fMax == fX -> (fMax, NE.cons x maxs)
                  | otherwise -> (fMax, maxs)
    scoredMap = HashMap.map getScores opFixityMap
    getScores :: HashMap FixityInfo (NonEmpty String) -> HashMap FixityInfo Int
    getScores = HashMap.map (sum . fmap (fromMaybe 0 . flip HashMap.lookup packageToPopularity))
    opFixityMap = unionsWith mergeOpFixityMaps (opFixityMapFrom <$> packagesMaps)
    unionsWith :: (Eq k, Hashable k) => (v -> v -> v) -> [HashMap k v] -> HashMap k v
    unionsWith f = \case
      [] -> HashMap.empty
      m : ms -> foldl' (HashMap.unionWith f) m ms
    mergeOpFixityMaps :: HashMap FixityInfo (NonEmpty String) -> HashMap FixityInfo (NonEmpty String) -> HashMap FixityInfo (NonEmpty String)
    mergeOpFixityMaps = HashMap.unionWith (<>)
    opFixityMapFrom :: (String, FixityMap) -> HashMap String (HashMap FixityInfo (NonEmpty String))
    opFixityMapFrom (packageName, opsMap) = HashMap.map (flip HashMap.singleton (packageName :| [])) opsMap

buildPackageFixityMap :: (String, PackageConfig) -> (String, FixityMap)
buildPackageFixityMap (packageName, config) =
  ( packageName,
    HashMap.filterWithKey
      ( case config of
          AllExcept xs -> \k _ -> k `notElem` xs
          NoneExcept xs -> \k _ -> k `elem` xs
      )
      $ fromMaybe HashMap.empty $
        HashMap.lookup packageName packageToOps
  )
