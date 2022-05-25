module Std
  (
    module Std,
    module Prelude,
    Map,
    Set,
    fold,
    toList,
    coerce,
    fromMaybe,
    Endo (..),
    join
  )
  where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as N
import Data.Foldable qualified
import Data.Foldable (fold, toList)
import Control.Arrow (first)
import Control.Applicative (liftA2)
import Data.List (intersperse)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo (..))
import Control.Monad (join)

(!=) :: (Eq a) => a -> a -> Bool
(!=) = (/=)

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

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
