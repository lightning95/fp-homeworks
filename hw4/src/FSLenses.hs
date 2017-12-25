{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module FSLenses
  ( getDirectory
  , FS(..)
  , dir
  , file
  , name
  , contents
  , _File
  , _Dir
  ) where

import           Control.Conditional (ifM)
import           Control.Lens
import           Control.Monad       (forM)
import           Data.Pointed        (Pointed (..))
import           System.Directory    (getDirectoryContents)
import           System.File.Tree    (isRealDir)
import           System.FilePath     ((</>))

data FS = Dir
        { _name     :: FilePath  -- название папки, не полный путь
        , _contents :: [FS]
        }
        | File
        { _name     :: FilePath  -- название файла, не полный путь
        } deriving (Show)

getDirectory :: FilePath -> IO FS
getDirectory p = Dir p <$> getChildren p
  where
    getChildren :: FilePath -> IO [FS]
    getChildren path = do
      cs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents path
      forM cs $ \c ->
        let c' :: FilePath
            c' = path </> c
        in ifM ( isRealDir c')
               ( Dir c <$> getChildren c')
               ( pure $ File c )

makeLenses ''FS
makePrisms ''FS

type Traversal01 s t a b =
  forall f . (Functor f, Pointed f) => (a -> f b) -> s -> f t

dir :: Traversal01 FS FS (FilePath, [FS]) (FilePath, [FS])
dir f (Dir n c) = uncurry Dir <$> f (n, c)
dir _ fileC     = point fileC

file :: Traversal01 FS FS FilePath FilePath
file f (File n) = File <$> f n
file _ dirC     = point dirC
