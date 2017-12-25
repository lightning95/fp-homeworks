{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module ChangeFS
  ( changeExt
  , subDirs
  , rmDir
  , getPath
  , move
  ) where

import           Control.Lens (Lens', Traversal', filtered, traversed, (%~),
                               (&), (.~), (^.), (^..), (^?), _1)
import           Data.List    (isSuffixOf, stripPrefix)
import           Data.Maybe   (isJust)
import           FSLenses     (FS (..), contents, name, _Dir, _File)
import           TraverseFS   (cd, ls)

changeExt :: String -> String -> FS -> FS
changeExt old new =
  let revOld = reverse old
      revNew = reverse new
  in contents
       .traversed
       .filtered (\x -> isJust (x ^? _File) && isSuffixOf old (x ^. name))
       .name %~ (\x ->
                  let rev    = reverse x
                      Just r = stripPrefix revOld rev
                  in reverse $ revNew ++ r)

subDirs :: FS -> [FilePath]
subDirs d = d ^.. ls ++ concatMap subDirs (d^..contents.traversed)

rmDir :: FilePath -> FS -> FS
rmDir arg d = d & contents .~ (d^..contents.traversed.filtered
    (\x -> not $ x ^? _Dir . _1 == Just arg && null (x^.contents) ) )

getPath :: Lens' FS FilePath
getPath = name

move :: FilePath -> Traversal' FS FS
move path f s = let n = s^.name
                in f (s & cd path.name %~ (n++))

