{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module ChangeFS
  ( changeExt
  , subDirs
  , rmDir
  ) where

import           Control.Lens (filtered, traversed, (%~), (&), (.~), (^.),
                               (^..))
import           Data.List    (isSuffixOf, stripPrefix)
import           FSLenses     (FS (..), contents, name)
import           TraverseFS   (ls)

changeExt :: String -> String -> FS -> FS
changeExt old new = let revOld = reverse old
                        revNew = reverse new
                    in contents.traversed.filtered(\x -> isSuffixOf old $ x^.name)
  .name%~(\x ->
            let rev    = reverse x
                Just r = stripPrefix revOld rev
            in reverse $ revNew ++ r)

subDirs :: FS -> [FilePath]
subDirs d = d^..ls ++ concatMap subDirs (d^..contents.traversed)

rmDir :: FilePath -> FS -> FS
rmDir arg d = d & contents .~ (d^..contents.traversed.filtered
    (\x -> not $ x^.name == arg && null (x^.contents) ) )
