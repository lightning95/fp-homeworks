{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module OptionalMonads
    ( Expr (..)
    , ArithmeticError (..)
    , eval
    , partial
    , total
    , apply
    , applyOrElse
    , withDefault
    , isDefinedAt
    , orElse
    , bin
    , combinations
    , permutations
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Category    as Cat (Category (..))
import           Control.Monad       (liftM2, replicateM, (>=>))
import           Data.Maybe          (fromMaybe, isJust)

data Expr = Const Int
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          deriving (Show)

data ArithmeticError = DivisionByZero
                     | PowerToNegative
                     deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = pure x
eval (Sum x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) = div <$> eval x <*> right
  where
    right :: Either ArithmeticError Int
    right = eval y >>= \a -> if a == 0 then Left DivisionByZero else pure a

eval (Pow x y) = (^) <$> eval x <*> right
  where
    right :: Either ArithmeticError Int
    right = eval y >>= \a -> if a < 0 then Left PowerToNegative else pure a

------------------------------------
data a ~> b
     = Partial   (a -> Maybe b) -- a partial function
     | Defaulted (a ~> b) b     -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total = partial . (Just .)

apply :: (a ~> b) -> a -> Maybe b
apply (Partial p)     a = p a
apply (Defaulted p b) a = case apply p a of
                            t@(Just _) -> t
                            _          -> Just b

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse (Partial f)     a b = fromMaybe b $ f a
applyOrElse (Defaulted f d) a _ = applyOrElse f a d

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault (Defaulted p _) = Defaulted p
withDefault p               = Defaulted p
-- Add a default value to a partial function. If the function was already
-- defaulted, override the value with the new default.

-- `BlackBird` by Amar Shah
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt = isJust ... apply

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f g = partial $ liftM2 (<|>) (f `apply`) (g `apply`)
-----------  partial $ \x -> on M.orElse (`apply` x) f g
-- Create a new partial function where the domain is the combination
-- of both partial functions. Priority is given to the first partial function
-- in case of conflict.

instance Cat.Category (~>) where
  id    = partial Just
  g . f = partial $ (f `apply`) >=> (g `apply`)
--   g . f = partial $ \x -> apply f x >>= (g `apply`)

----------------------------
bin :: Int -> [ [ Int ] ]
bin = flip replicateM [ 0, 1 ]

combinations :: Int -> Int -> [ [ Int ] ]
combinations _ 0 = [ [] ]
combinations 0 _ = []
combinations n k = ((++ [n]) <$> combinations (n - 1) (k - 1)) ++ combinations (n - 1) k

permutations :: forall a . [ a ] -> [ [ a ] ]
permutations []       = [ [] ]
permutations (x : xs) = permutations xs >>= \t -> f [] t
  where
    f :: [a] -> [a] -> [ [ a ] ]
    f a []       = [a ++ [ x ]]
    f a (b : bs) = (a ++ [ x ] ++ (b : bs)): f (a ++ [ b ]) bs
