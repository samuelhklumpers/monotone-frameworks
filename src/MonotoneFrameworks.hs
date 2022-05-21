{-# language NoImplicitPrelude #-}

module MonotoneFrameworks where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Std

class (Monoid m, Eq m) => BoundedSemiLattice m where
  lessOrEquals :: m -> m -> Bool
  lessOrEquals a b = a <> b == b

bottom :: (BoundedSemiLattice m) => m
bottom = mempty

-- instance (BoundedSemiLattice v) => BoundedSemiLattice (Map k v) where
--   lessOrEquals = undefined -- MM.merge
--   bottom = M.empty

newtype Label =
  Label Int
  deriving (Show, Eq, Ord)

data MonotoneFramework propertySpace =
  MonotoneFramework
    {
      flow :: Map Label (Set Label),
      extremalLabels :: Set Label,
      extremalValue :: propertySpace,
      transferFunctions :: Map Label (propertySpace -> propertySpace)
    }

-- | warning. partial. 'transferFunctions' must cover all labels.
--
-- warning. termination is only guaranteed if 'propertySpace' satisfies the
-- /Ascending Chain Condition/.
mfpSolution ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  MonotoneFramework propertySpace ->
  -- | \(\mathrm{Analysis}_\circ\) and \(\mathrm{Analysis}_\bullet\)
  (Map Label propertySpace, Map Label propertySpace)
mfpSolution
  (MonotoneFramework flow extremalLabels extremalValue transferFunctions)
  = (analysisEntry, M.mapWithKey transferFunction analysisEntry)
  where
    (_, analysisEntry) =
      iterateFinite step (initialAnalysis, initialWorkList)
    initialAnalysis =
      M.fromSet (const extremalValue) extremalLabels
      <> -- left biased union
      M.fromSet (const bottom) (M.keysSet flow <> fold flow)
    initialWorkList =
      foldMap (\(l, ls) -> (l,) <$> toList ls) (M.toList flow)
    step ::
      -- | pair of current analysis and worklist
      (Map Label propertySpace, [(Label, Label)]) ->
      -- | either result in case of termination or new state
      Either
        (Map Label propertySpace) -- terminate
        (Map Label propertySpace, [(Label, Label)])
    step (analysisOld, (l, l') : workListRest)
      |
        transferFunction l (analysisLookup l analysisOld)
        `lessOrEquals`
        analysisLookup l' analysisOld
        = Right (analysisOld, workListRest)
      | otherwise =
        Right $
        (
          M.adjust
            (<> transferFunction l (analysisLookup l analysisOld))
            l'
            analysisOld
          ,
          fmap (l',) $ toList $ fromMaybe S.empty $ M.lookup l' flow
        )
    step (analysis, []) = Left analysis -- terminate
    -- | warning. partial
    transferFunction :: Label -> propertySpace -> propertySpace
    transferFunction l =
      fromMaybe (error $ "transfer function missing for " <> show l) $
      M.lookup l transferFunctions
    -- | warning. partial
    analysisLookup :: Label -> Map Label propertySpace -> propertySpace
    analysisLookup =
      fromMaybe (error "impossible. initialAnalysis covers all labels") .:
      M.lookup
