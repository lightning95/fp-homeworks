{-# LANGUAGE TypeOperators #-}

module Lib
    ( Expr
    , ArithmeticError
    , eval
    , partial
    , total
    , apply
    , bin
    , combinations
    , permutations
    ) where

data Expr = Const Int
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          deriving (Show)

data ArithmeticError = DivisionByZero
                     | PowerToNegative
                     deriving (Show)

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = pure x
eval (Sum x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) =
  let right = (eval y >>= \a -> if a == 0 then Left DivisionByZero else pure a)
  in div <$> eval x <*> right
eval (Pow x y) =
  let right = (eval y >>= \a -> if a < 0 then Left PowerToNegative else pure a)
  in (^) <$> eval x <*> right

data a ~> b
     = Partial   (a -> Maybe b) -- a partial function
     | Defaulted (a ~> b) b     -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total = partial . (Just .)

apply :: (a ~> b) -> a -> Maybe b
apply (Partial p) a = p a
apply (Defaulted p b) a = case apply p a of
                          t@(Just _) -> t
                          _          -> Just b

-- applyOrElse :: (a ~> b) -> a -> b -> b
-- withDefault :: (a ~> b) -> b -> (a ~> b)  -- Add a default value to a partial function. If the function was already
--                                           -- defaulted, override the value with the new default.
-- isDefinedAt :: (a ~> b) -> a -> Bool
-- orElse      :: (a ~> b) -> (a ~> b) -> a ~> b  -- Create a new partial function where the domain is the combination
--                                                -- of both partial functions. Priority is given to the first partial function
--                                                -- in case of conflict.
bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \x -> [0 : x, 1 : x]
-- bin = flip replicateM [0, 1]

combinations :: Int -> Int -> [[Int]]
combinations _ 0 = [[]]
combinations 0 _ = []
combinations n k = ((n:) <$> combinations (n - 1) (k - 1)) ++ combinations (n - 1) k

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = permutations xs >>= \t -> f [] t
  where
    f a [] = [a ++ [x]]
    f a (b:bs) = (a ++ [x] ++ (b:bs)): f (a++[b]) bs
