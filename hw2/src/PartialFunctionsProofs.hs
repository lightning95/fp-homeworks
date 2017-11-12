module PartialFunctions where

-- "identity/left" forall p .
--                 id . p = p
-- "identity/right" forall p .
--                 p . id = p
-- "association"   forall p q r .
--                 (p . q) . r = p . (q . r)

-- instance Cat.Category (~>) where
--   id :: a ~> a
--   id    = partial Just
--   (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)
--   g . f = partial $ (f `apply`) >=> (g `apply`)

--id . p                                                       (id)
--   = partial Just . p                                        (.)
--   = partial $ (p `apply` ) >=> ((partial Just) `apply`)     (>=>)
--   = partial $ \x -> apply p x >>= ((partial Just) `apply`)  (apply)
--   = partial $ \x -> apply p x >>= Just                      (>>= Just == id)
--   = partial $ \x -> apply p x                               (\x -> f x => f)
--   = partial $ apply p
--   = (partial . apply) p
--   = p

--p . id
--   = p . partial Just                                      (id)
--   = partial $ ((partial Just) `apply`) >=> (p `apply`)    (apply)
--   = partial $ Just >=> (p `apply`)                        (>=>)
--   = partial $ \x -> Just x >>= (p `apply`)                (>>=)
--   = partial $ \x -> apply p x                             (apply)
--   = partial $ apply p
--   = (partial . apply) p
--   = p

-- (p . q) . r
--    = partial $ (r `apply`) >=> (partial $ (q `apply`) >=> (p `apply`)) `apply`             (>=>)
--    = partial $ \x -> apply r x >>=1 ((partial $ \y -> apply q y >>=2 (p `apply`)) `apply`) (>>= 1)
--    = partial $ \x -> (partial $ \y -> apply q y >>= (p `apply`)) `apply` (apply r x)       (>>=)
--    = partial $ \x -> (partial $ \y -> p `apply` (apply q y)) `apply` (apply r x)           (\x)
--    = partial $ \x -> apply p (apply q (apply r x))

-- p . (q . r)
--    = partial $ ((partial $ (r `apply `) >=> (q `apply`)) `apply`) >=> (p `apply`)            (>=>)
--    = partial $ \x -> (partial $ \y -> apply r y >>=1 (q `apply`)) `apply` x >>= (p `apply`)  (>>=1)
--    = partial $ \x -> (partial $ \y -> q `apply` (apply r y)) `apply` x >>= (p `apply`)       (apply)
--    = partial $ \x -> apply (apply q (apply r x)) >>= (p `apply`)                             (>>=)
--    = partial $ \x -> apply p (apply q (apply r x))                                           (>>=)
