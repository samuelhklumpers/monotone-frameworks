{-# language NoImplicitPrelude #-}

module Std
  (
    module Std,
    module Relude,
    module Relude.Extra.Foldable1,
    module Control.Exception.Safe
  )
  where

import Control.Exception.Safe
import Data.List.NonEmpty qualified as N
import Data.Map qualified as M
import Data.Foldable qualified
import Relude.Extra.Foldable1 hiding (foldr1)
import Relude hiding (intercalate, some)

(!=) :: (Eq a) => a -> a -> Bool
(!=) = (/=)

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

foldr1 :: (Foldable1 f) => (a -> a -> a) -> f a -> a
foldr1 f = Data.Foldable.foldr1 f . toNonEmpty

iterateFinite :: (a -> Either b a) -> a -> ([a], b)
iterateFinite f =
  go
  where
    go aOld
      | Right aNew <- f aOld = first (aNew :) (go aNew)
      | Left b <- f aOld = ([], b)

intercalate :: (Monoid a, Foldable t) => a -> t a -> a
intercalate a = fold . intersperse a . toList

infixl 4 <<*>>
(<<*>>) ::
  (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

some :: Alternative f => f a -> f (NonEmpty a)
some v = (:|) <$> v <*> many v

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = uncurry f =<< liftA2 (,) a b

sortNonEmptyOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortNonEmptyOn f =
  fmap snd .
  N.sortBy (comparing fst) .
  fmap (\a -> let b = f a in b `seq` (b, a))

-- | fails if the second argument is 'minBound'
fromToUnsafe :: (Enum a) => a -> a -> [a]
fromToUnsafe lower upper = enumFromTo lower (pred upper)
