{-# language NoImplicitPrelude #-}

module MonotoneFrameworks where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Map.Merge.Strict qualified as MM
import Std

class (Monoid m, Eq m) => BoundedSemiLattice m where
  lessOrEquals :: m -> m -> Bool
  lessOrEquals a b = a <> b == b

bottom :: (BoundedSemiLattice m) => m
bottom = mempty

type Label = Int

data MonotoneFramework propertySpace =
  MonotoneFramework
    {
      flow :: Map Label (Set Label),
      extremalLabels :: Set Label,
      extremalValue :: propertySpace,
      transferFunctions :: Map Label (propertySpace -> propertySpace)
    }

-- | intraprocedural MFP solution
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

-- invariant. call label is functionally dependent on the return label
newtype InterproceduralFragment propertySpace =
  InterproceduralFragment
    (Map
      Label -- call label
      (
        Label -- return label
        ,
        (propertySpace -> propertySpace -> propertySpace) -- transfer function
      )
    )

-- | interprocedural MFP solution
-- | warning. partial. 'transferFunctions' must cover all labels except for
-- call return labels.
--
-- warning. termination is only guaranteed if 'propertySpace' satisfies the
-- /Ascending Chain Condition/.
mfpSolution' ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  MonotoneFramework propertySpace ->
  InterproceduralFragment propertySpace ->
  -- | \(\mathrm{Analysis}_\circ\) and \(\mathrm{Analysis}_\bullet\)
  (
    Map Label (ContextSensitive propertySpace),
    Map Label (ContextSensitive propertySpace)
  )
mfpSolution'
  (MonotoneFramework flow extremalLabels extremalValue transferFunctions)
  interproceduralFragment
  = (analysisEntry, M.mapWithKey transferFunction analysisEntry)
  where
    (_, analysisEntry) =
      iterateFinite step (initialAnalysis, initialWorkList)
    initialAnalysis =
      M.fromSet (const $ TotalMapOnBoundedSemiLattice $ M.singleton [] extremalValue) extremalLabels
      <> -- left biased union
      M.fromSet (const bottom) (M.keysSet flow <> fold flow)
    initialWorkList =
      foldMap (\(l, ls) -> (l,) <$> toList ls) (M.toList flow)
    step ::
      -- | pair of current analysis and worklist
      (Map Label (ContextSensitive propertySpace), [(Label, Label)]) ->
      -- | either result in case of termination or new state
      Either
        (Map Label (ContextSensitive propertySpace)) -- terminate
        (Map Label (ContextSensitive propertySpace), [(Label, Label)])
    step (analysisOld, (l, l') : workListRest)
      | newPropertiesForL' `lessOrEquals` analysisLookup l' analysisOld =
        Right (analysisOld, workListRest)
      | otherwise =
        Right $
        (
          M.adjust (<> newPropertiesForL') l' analysisOld
          ,
          (
            (case lookupCall l' interproceduralFragment of
              Nothing -> []
              Just (returnLabel, _) -> outgoingFlow returnLabel
            )
            <>
            outgoingFlow l'
          )
        )
      where
        newPropertiesForL'
          | Just _ <- lookupCall l interproceduralFragment =
            transferFunction l (analysisLookup l analysisOld) -- to-do. call string manipulation
          | Just (callLabel, f) <- lookupReturn l interproceduralFragment =
            zipTotalMapOnBoundedSemiLatticeWith f
              (analysisLookup callLabel analysisOld)
              (analysisLookup l analysisOld) -- to-do. call string manipulation
          | otherwise = transferFunction l (analysisLookup l analysisOld)
    step (analysis, []) = Left analysis -- terminate
    outgoingFlow :: Label -> [(Label, Label)]
    outgoingFlow l =
      fmap (l,) $
      toList $
      fromMaybe S.empty $
      M.lookup l flow
    -- | warning. partial
    transferFunction ::
      Label ->
      ContextSensitive propertySpace ->
      ContextSensitive propertySpace
    transferFunction l =
      fmap $
      fromMaybe (error $ "transfer function missing for " <> show l) $
      M.lookup l transferFunctions
    -- | warning. partial
    analysisLookup :: Label -> Map Label v -> v
    analysisLookup =
      fromMaybe (error "impossible. initialAnalysis covers all labels") .:
      M.lookup

newtype TotalMapOnBoundedSemiLattice domain codomain =
  TotalMapOnBoundedSemiLattice (Map domain codomain)
  deriving (Eq)
  deriving (Functor) via Map domain

($$) ::
  (BoundedSemiLattice codomain, Ord domain) =>
  TotalMapOnBoundedSemiLattice domain codomain -> domain -> codomain
($$) (TotalMapOnBoundedSemiLattice m) = fromMaybe bottom . (`M.lookup` m)

-- | requires @f bottom bottom == bottom@. to-do: can we assume that?
zipTotalMapOnBoundedSemiLatticeWith ::
  forall a b c domain.
  (
    BoundedSemiLattice a,
    BoundedSemiLattice b,
    BoundedSemiLattice c,
    Ord domain
  ) =>
  (a -> b -> c) ->
  TotalMapOnBoundedSemiLattice domain a ->
  TotalMapOnBoundedSemiLattice domain b ->
  TotalMapOnBoundedSemiLattice domain c
zipTotalMapOnBoundedSemiLatticeWith f =
  coerce @(Map domain a -> Map domain b -> Map domain c) $
  MM.merge
    (MM.mapMissing (\_ a -> f a bottom))
    (MM.mapMissing (\_ b -> f bottom b))
    (MM.zipWithMatched (\_ a b -> f a b))

instance
  (Semigroup codomain, Ord domain) =>
  Semigroup (TotalMapOnBoundedSemiLattice domain codomain)
  where
    (<>) =
      coerce -- ignore
        @(Map domain codomain -> Map domain codomain -> Map domain codomain) $
        M.unionWith (<>)
instance
  (Semigroup codomain, Ord domain) =>
  Monoid (TotalMapOnBoundedSemiLattice domain codomain)
  where
    mempty =
      coerce @(Map domain codomain) $ -- ignore
        M.empty
instance
  (BoundedSemiLattice codomain, Ord domain) =>
  BoundedSemiLattice (TotalMapOnBoundedSemiLattice domain codomain)
  where
    lessOrEquals =
      coerce --- ignore
        @(Map domain codomain -> Map domain codomain -> Bool) $ -- ignore
        null
        .:
        (MM.merge
          -- allow bottom to be missing from right hand side
          (MM.mapMaybeMissing
            (\_ a -> if a == bottom then Nothing else Just a)
          )
          MM.dropMissing -- allow any missing from left hand side
          (MM.zipWithMaybeMatched
            (\_ a b -> if a `lessOrEquals` b then Nothing else Just a)
          )
        )

type ContextSensitive = TotalMapOnBoundedSemiLattice [Label]

lookupCall ::
  Label ->
  InterproceduralFragment propertySpace ->
  Maybe (Label, (propertySpace -> propertySpace -> propertySpace))
lookupCall = undefined -- to-do.

lookupReturn ::
  Label ->
  InterproceduralFragment propertySpace ->
  Maybe (Label, (propertySpace -> propertySpace -> propertySpace))
lookupReturn = undefined -- to-do.
