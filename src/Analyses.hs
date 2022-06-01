module Analyses where

import Data.Map.Strict qualified as M
import Data.Maybe qualified as Maybe
import Std (first, second)
import Control.Arrow ((&&&))

-- | Transfer functions per label, grouped by unary and binary
type DifTrans m = (M.Map Int (m -> m), M.Map Int (m -> m -> m))

-- | Insert a unary transfer function at the label
insertL :: Int -> (m -> m) -> DifTrans m -> DifTrans m
insertL = (first .) . M.insert

-- | Insert a binary transfer function at the label
insertR :: Int -> (m -> m -> m) -> DifTrans m -> DifTrans m
insertR = (second .) . M.insert

-- | Create a singleton "Map" with this unary transfer function at the label
singleL :: Int -> (m -> m) -> DifTrans m
singleL i x = insertL i x mempty

-- | Create a singleton "Map" with this binary transfer function at the label
singleR :: Int -> (m -> m -> m) -> DifTrans m
singleR i x = insertR i x mempty

-- | Lookup a unary transfer function
lookupL :: Int -> DifTrans p -> (p -> p)
lookupL i (m, _) = Maybe.fromMaybe (error $ show i ++ " is not in " ++ show (M.keys m)) $ M.lookup i m

-- | Lookup a binary transfer function
lookupR :: Int -> DifTrans p -> (p -> p -> p)
lookupR i (_, m) = Maybe.fromMaybe (error $ show i ++ " is not in " ++ show (M.keys m)) $ M.lookup i m

-- | Internal representation of "Proc'" as a record, for convenience
data Proc'' = Proc'' { procEntry :: Int, procExit :: Int, procName :: String, procInp :: [String], procOut :: String } deriving (Show, Eq, Ord)

-- | Type of procedure lookup tables
type DStar = [(String, Proc'')]

type Edge = (Int, Int)
data Inter = Inter Int Int Int Int deriving (Eq, Ord)

instance Show Inter where
  show (Inter a b c d) = show (a, b, c, d)

-- | Unsafely lookup a procedure by its name
findProc :: String -> DStar -> Proc''
findProc = (Maybe.fromJust .) . lookup

-- | Group by a key function
groupBy :: Ord k => (a -> k) -> [a] -> M.Map k [a]
groupBy f = M.fromListWith (++) . map (f &&& pure)

-- | Group by the keys of a key/value function
groupBy' :: Ord k => (a -> (k, b)) -> [a] -> M.Map k [b]
groupBy' f = M.fromListWith (++) . map (fst . f &&& pure . (snd . f))

concatSnd :: [(k, [(m, v)])] -> [(k, (m, v))]
concatSnd = (h =<<)
  where
    h (k, xs) = fmap (k, ) xs

-- | Flip the inner and outer keys of a nested map
flipMap :: (Ord k, Ord m) => M.Map k (M.Map m v) -> M.Map m (M.Map k v)
flipMap = fmap M.fromList . groupBy' h . concatSnd . M.toList . fmap M.toList
  where
    h (k, (m, v)) = (m, (k, v))

