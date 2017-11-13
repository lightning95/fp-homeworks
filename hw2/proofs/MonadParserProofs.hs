module MonadParserProofs where

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
---------------------------------------------------------
-- instance Functor Parser where
--   fmap f (Parser p) = Parser $ fmap (first f) . p

-- functor laws
-- 1) fmap id        ==  id
-- 2) fmap (f . g)  ==  fmap f . fmap g

-- fmap id (Parser p)
--   = Parser $ fmap (first id) . p
--   = Parser $ \s -> fmap (first id) (p s)
--   = Parser $ \s -> p s
--   = Parser p
-------------------------------------------------------
-- instance Applicative Parser where
--   pure a                  = Parser $ \s -> Just (a, s)
--   Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
--       Just (f, rest) -> first f <$> p2 rest
--       _              -> Nothing

-- 1) identity
--   pure id <*> v              = v
-- 2) composition
--   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 3) homomorphism
--   pure f <*> pure x          = pure (f x)
-- 4) interchange
--   u <*> pure y               = pure ($ y) <*> u

-- pure id <*> (Parser p)
--   = Parser $ \s -> case (\t -> Just (id, t)) s of         (t = s)
--       Just (id, t) -> first id <$> p t
--   = Parser $ \s -> first id <$> p s
--   = Parser $ \s -> fmap (first id) (p s)
--   = Parser $ \s -> p s
--   = Parser p
--------------------------------------------------------
-- instance Monad Parser where
--   Parser p >>= f = Parser $ \s -> case p s of
--     Just (res, rest) -> runParser (f res) rest
--     _                -> Nothing

-- 1) return a >>= k           =  k a
-- 2) m >>= return             =  m
-- 3) m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
--
-- return a >>= k                                         (return == pure)
--   = (Parser $ \s -> Just (a, s)) >>= k                 (>>=)
--   = Parser $ \t -> case (\s -> Just (a, s)) t of       (case)
--        Just (res, rest) -> runParser (k res) rest
--   = Parser $ \t -> runParser (k a) t                   (\x)
--   = Parser $ runParser (k a)
--   = k a
