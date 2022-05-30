{-# language NoImplicitPrelude #-}

module MonotoneFrameworks where

import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Map.Merge.Strict qualified as MM
import Std

callStringsLimit :: Int
callStringsLimit = 1

class (Monoid m, Eq m) => BoundedSemiLattice m where
  lessOrEquals :: m -> m -> Bool
  lessOrEquals a b = a <> b == b

instance BoundedSemiLattice m => BoundedSemiLattice (Maybe m)

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

-- invariant. call label is functionally dependent on the return label
newtype InterproceduralFragment propertySpace =
  InterproceduralFragment
    (Map
      Label -- call label in the case of forward flow
      (
        Label -- return label in the case of forward flow
        ,
        -- transfer function
        (propertySpace {-call-} -> propertySpace {-procedure-} -> propertySpace)
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
  -- | \(\mathrm{Analysis}_\circ\), \(\mathrm{Analysis}_\bullet\), and all
  -- intermediate steps
  (
    Map Label (ContextSensitive propertySpace),
    Map Label (ContextSensitive propertySpace),
    [(Map Label (ContextSensitive propertySpace), [(Label, Label)])]
  )
mfpSolution'
  (MonotoneFramework flow extremalLabels extremalValue transferFunctions)
  interproceduralFragment
  = (
    analysisEntry,
    M.mapWithKey (\k _ -> transfer analysisEntry k) analysisEntry,
    s
  )
  where
    (s, analysisEntry) =
      first
        ((initialAnalysis, initialWorkList) :)
        (iterateFinite step (initialAnalysis, initialWorkList))
    initialAnalysis =
      M.fromSet (const $ ContextSensitive $ M.singleton [] extremalValue) extremalLabels
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
      | transfer analysisOld l `lessOrEquals` analysisLookup l' analysisOld =
        Right (analysisOld, workListRest)
      | otherwise =
        Right $
        (
          M.adjust (<> transfer analysisOld l) l' analysisOld
          ,
          (
            outgoingFlow l' <>
            (case lookupCall l' interproceduralFragment of
              Nothing -> []
              Just (returnLabel, _) -> outgoingFlow returnLabel
            ) <>
            workListRest
          )
        )
    step (analysis, []) = Left analysis -- terminate
    transfer ::
      Map Label (ContextSensitive propertySpace) ->
      Label ->
      ContextSensitive propertySpace
    transfer analysisOld l
      | Just _ <- lookupCall l interproceduralFragment =
        coerce -- ignore
          @(Map [Label] propertySpace -> Map [Label] propertySpace)
          (M.mapKeysWith (<>) (take callStringsLimit . (l :))) $
          transferFunction l (analysisLookup l analysisOld)
      | Just (callLabel, f) <- lookupReturn l interproceduralFragment =
        coerce -- ignore
          @(([Label] -> propertySpace -> propertySpace) -> Map [Label] propertySpace -> Map [Label] propertySpace)
          M.mapWithKey
            (\s p ->
              f
                p
                (fromMaybe bottom $
                  lookupContext
                    (take callStringsLimit (callLabel : s))
                    (analysisLookup l analysisOld)
                )
            )
            (analysisLookup callLabel analysisOld)
      | otherwise = transferFunction l (analysisLookup l analysisOld)
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

newtype ContextSensitive propertySpace =
  ContextSensitive {runTotalMap :: Map [Label] propertySpace}
  deriving (Show, Eq)
  deriving (Functor) via Map [Label]

lookupContext ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  [Label] -> ContextSensitive propertySpace -> Maybe propertySpace
lookupContext =
  coerce -- ignore
    @([Label] -> Map [Label] propertySpace -> Maybe propertySpace) -- ignore
    M.lookup

instance
  (Semigroup propertySpace) =>
  Semigroup (ContextSensitive propertySpace)
  where
    (<>) =
      coerce -- ignore
        @(Map [Label] propertySpace -> Map [Label] propertySpace -> Map [Label] propertySpace) $
        M.unionWith (<>)
instance
  (Semigroup propertySpace) =>
  Monoid (ContextSensitive propertySpace)
  where
    mempty =
      coerce @(Map [Label] propertySpace) -- ignore
        M.empty
instance
  (BoundedSemiLattice propertySpace) =>
  BoundedSemiLattice (ContextSensitive propertySpace)
  -- to-do. efficient implementation

lookupCall ::
  forall propertySpace.
  Label ->
  InterproceduralFragment propertySpace ->
  Maybe (Label, (propertySpace -> propertySpace -> propertySpace))
lookupCall =
  coerce -- ignore
    @(Label -> Map Label (Label, (propertySpace -> propertySpace -> propertySpace)) -> (Maybe (Label, (propertySpace -> propertySpace -> propertySpace))))
    M.lookup

lookupReturn ::
  Label ->
  InterproceduralFragment propertySpace ->
  Maybe (Label, (propertySpace -> propertySpace -> propertySpace))
lookupReturn returnLabel (InterproceduralFragment interproceduralFragment)
  = case
    M.toList $ M.filter ((== returnLabel) . fst) $ interproceduralFragment
  of
    [] -> Nothing
    [(callLabel, (_returnLabel, transferFunction))] ->
      Just (callLabel, transferFunction)
    _ -> error "impossible. smart constructor of InterproceduralFragment."
