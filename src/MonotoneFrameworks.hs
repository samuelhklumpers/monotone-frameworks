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

type Label = Int

data MonotoneFramework propertySpace =
  MonotoneFramework
    {
      flow :: Map Label (Set Label),
      extremalLabels :: Set Label,
      extremalValue :: propertySpace,
      transferFunctions :: Map Label (propertySpace -> propertySpace),
      interproceduralFragment :: InterproceduralFragment propertySpace
    }

-- | invariant. call label is functionally dependent on the return label
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
--
-- warning. partial. 'transferFunctions' must cover all labels except for
-- call return labels.
--
-- warning. termination is only guaranteed if 'propertySpace' satisfies the
-- /Ascending Chain Condition/.
mfpSolution ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  -- | monotone framework
  MonotoneFramework propertySpace ->
  -- | \(\mathrm{Analysis}_\circ\), \(\mathrm{Analysis}_\bullet\), and all
  -- intermediate steps
  (
    Map Label propertySpace,
    Map Label propertySpace,
    [(Map Label propertySpace, [(Label, Label)])]
  )
mfpSolution
  (MonotoneFramework flow extremalLabels extremalValue transferFunctions interproceduralFragment)
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
      Map Label propertySpace ->
      Label ->
      propertySpace
    transfer analysisOld l
      | Just (callLabel, f) <- lookupReturn l interproceduralFragment =
        f
          (analysisLookup callLabel analysisOld)
          (analysisLookup l analysisOld)
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
      propertySpace ->
      propertySpace
    transferFunction l =
      fromMaybe (error $ "transfer function missing for " <> show l) $
      M.lookup l transferFunctions
    -- | warning. partial
    analysisLookup :: Label -> Map Label v -> v
    analysisLookup =
      fromMaybe (error "impossible. initialAnalysis covers all labels") .:
      M.lookup

mapFunctionWithCallLabel ::
  forall p0 p1.
  (Label -> (p0 -> p0 -> p0) -> (p1 -> p1 -> p1)) ->
  InterproceduralFragment p0 ->
  InterproceduralFragment p1
mapFunctionWithCallLabel binaryTransferFunction =
  coerce -- ignore
    @(Map Label (Label, (p0 -> p0 -> p0)) -> Map Label (Label, (p1 -> p1 -> p1))) $
    M.mapWithKey (second . binaryTransferFunction)

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
    [(callLabel, (_returnLabel, binaryTransferFunction))] ->
      Just (callLabel, binaryTransferFunction)
    _ -> error "impossible. smart constructor of InterproceduralFragment." -- to-do
