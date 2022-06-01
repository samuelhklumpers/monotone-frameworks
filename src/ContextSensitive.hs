{-# language NoImplicitPrelude #-}

module ContextSensitive where

import MonotoneFrameworks
  ( BoundedSemiLattice,
    Label,
    MonotoneFramework (MonotoneFramework),
    bottom,
    lookupCall,
    mapFunctionWithCallLabel,
  )

import Data.Map.Strict qualified as M
import Std

newtype ContextSensitive propertySpace =
  ContextSensitive {runTotalMap :: Map [Label] propertySpace}
  deriving (Show, Eq)
  deriving (Functor) via Map [Label]

contextSensitize ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  Int ->
  MonotoneFramework propertySpace ->
  MonotoneFramework (ContextSensitive propertySpace)
contextSensitize
  callStringsLimit
  (MonotoneFramework flow extremalLabels extremalValue transferFunctions interproceduralFragment)
  = MonotoneFramework
    flow
    extremalLabels
    (ContextSensitive $ M.singleton [] extremalValue)
    (M.mapWithKey
      (\l transferFunction ->
        case lookupCall l interproceduralFragment of
          Nothing -> transferFunction
          Just _ ->
            mapContexts (take callStringsLimit . (l :))
            .
            transferFunction
      )
      (fmap @(Map Label) (fmap @ContextSensitive) transferFunctions)
    )
    (mapFunctionWithCallLabel
      (\callLabel binaryTransferFunction propertyCall propertyReturn ->
        mapWithContext
          (\callString p ->
            binaryTransferFunction
              p
              (fromMaybe bottom $
                lookupContext
                  (take callStringsLimit (callLabel : callString))
                  propertyReturn
              )
          )
          propertyCall
      )
      interproceduralFragment
    )

mapWithContext ::
  forall propertySpace0 propertySpace1.
  ([Label] -> propertySpace0 -> propertySpace1) ->
  ContextSensitive propertySpace0 ->
  ContextSensitive propertySpace1
mapWithContext =
  coerce -- ignore
    @(([Label] -> propertySpace0 -> propertySpace1) -> Map [Label] propertySpace0 -> Map [Label] propertySpace1)
    M.mapWithKey

lookupContext ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  [Label] -> ContextSensitive propertySpace -> Maybe propertySpace
lookupContext =
  coerce -- ignore
    @([Label] -> Map [Label] propertySpace -> Maybe propertySpace) -- ignore
    M.lookup

mapContexts ::
  forall propertySpace.
  (BoundedSemiLattice propertySpace) =>
  ([Label] -> [Label]) ->
  ContextSensitive propertySpace ->
  ContextSensitive propertySpace
mapContexts =
  coerce -- ignore
    @(([Label] -> [Label]) -> Map [Label] propertySpace -> Map [Label] propertySpace) $
    M.mapKeysWith (<>)

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
