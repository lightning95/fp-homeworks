{-# LANGUAGE TemplateHaskell #-}

module ShowT
  ( mkShowT
  , ShowT(..)
  ) where

import qualified Data.Text           as T (Text, append, intercalate, pack)
import           Language.Haskell.TH (Con (..), Dec (..), Exp, Info (..), Name,
                                      Q, conT, listE, nameBase, reify, varE)

class ShowT a where
  showT :: a -> T.Text

mkShowT :: Name -> Q [Dec]
mkShowT dName = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify dName

    let names :: [Name]
        names = map (\ (name, _, _) -> name) fields

    let showField :: Name -> Q Exp
        showField name = [|\x -> s ++ " = " ++ show ($(varE name) x)|]
           where s = nameBase name

    let showFields :: Q Exp
        showFields = listE $ map showField names

    [d|instance ShowT $(conT dName) where
        showT x = T.pack "ShowT: " `T.append`
          T.intercalate (T.pack ", ") (map (T.pack . ($ x)) $showFields)|]
