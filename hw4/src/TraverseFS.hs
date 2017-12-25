{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module TraverseFS
  ( cd
  , ls
  , fileM
  ) where

import           Control.Lens (Traversal', filtered, traversed, (^?), _1)
import           FSLenses     (FS, contents, name, _Dir, _File)

cd :: FilePath -> Traversal' FS FS
cd path = contents.traversed.filtered (\x -> x ^? _Dir . _1 == Just path)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

fileM :: FilePath -> Traversal' FS FilePath
fileM n = contents.traversed.filtered (\x-> x ^? _File == Just n).name
