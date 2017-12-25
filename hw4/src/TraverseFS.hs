{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module TraverseFS
  ( cd
  , ls
  , fileM
  ) where

import           Control.Lens (Traversal', filtered, traversed, (^.))
import           FSLenses     (FS, contents, name)

cd :: FilePath -> Traversal' FS FS
cd path = contents.traversed.filtered (\x -> x^.name == path)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

fileM :: FilePath -> Traversal' FS FS
fileM n = contents.traversed.filtered (\x -> x^.name == n)
