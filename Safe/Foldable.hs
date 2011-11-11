{- |
Equivalent versions to the "Safe" module, but generalised to work
over any 'Foldable' type.
-}
module Safe.Foldable where

import Data.Foldable
import Data.Monoid
import Data.Maybe
import Prelude hiding (foldl, foldr)

mfl :: (a -> a -> a) -> Maybe a -> a -> Maybe a
mfl _ Nothing x = Just x
mfl fun (Just y) x = Just (fun y x)

-- |
-- > Same as Data.Foldable.foldl1
foldl1Note :: Foldable t => String -> (a -> a -> a) -> t a -> a
foldl1Note msg fun fld = fromMaybe (error $ "Safe.Foldable.foldl1Note: empty list, " ++ msg) (foldl (mfl fun) Nothing fld)

foldl1Def :: Foldable t => a -> (a -> a -> a) -> t a -> a
foldl1Def def fun fld = fromMaybe def (foldl (mfl fun) Nothing fld)

foldl1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldl1May fun = foldl (mfl fun) Nothing

-- | Default value is the mempty from a monoid
foldl1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldl1Safe fun = foldl fun mempty


mfr :: (a -> a -> a) -> a -> Maybe a -> Maybe a
mfr _ x Nothing = Just x
mfr fun x (Just y) = Just (fun x y)

-- |
-- > Same as Data.Foldable.foldr1
foldr1Note :: Foldable t => String -> (a -> a -> a) -> t a -> a
foldr1Note msg fun fld = fromMaybe (error $ "Safe.Foldable.foldr1Note: empty list, " ++ msg) (foldr (mfr fun) Nothing fld)

foldr1Def :: Foldable t => a -> (a -> a -> a) -> t a -> a
foldr1Def def fun fld = fromMaybe def (foldr (mfr fun) Nothing fld)

foldr1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldr1May fun = foldr (mfr fun) Nothing

-- | Default value is the mempty from a monoid
foldr1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldr1Safe fun = foldr fun mempty

-- |
-- > Same as Data.Foldable.find

findJust :: Foldable t => (a -> Bool) -> t a -> a
findJust op fld = fromMaybe (error "Safe.Foldable.findJust, item not found") (find op fld)

findJustDef :: Foldable t => a -> (a -> Bool) -> t a -> a
findJustDef def op fld = fromMaybe def (find op fld)

findJustNote :: Foldable t => String -> (a -> Bool) -> t a -> a
findJustNote msg op fld = fromMaybe (error $ "Safe.Foldable.findJustNote: element not found, " ++ msg) (find op fld)

-- | Default value is the mempty from a monoid
findJustSafe :: (Monoid m, Foldable t) => (m -> Bool) -> t m -> m
findJustSafe op fld = fromMaybe mempty (find op fld)


