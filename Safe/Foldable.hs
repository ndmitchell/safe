{- |
Versions of 'Foldable' functions, with wrappers like the "Safe" module.
-}
module Safe.Foldable(
    -- * New functions
    findJust,
    -- * Safe wrappers
    foldl1Note, foldl1Def, foldl1May,
    foldr1Note, foldr1Def, foldr1May,
    findJustNote, findJustDef,
    -- * Deprecated
    foldl1Safe, foldr1Safe, findJustSafe
    ) where

import Data.Foldable
import Data.Monoid
import Data.Maybe
import Prelude hiding (foldl, foldr)

mfl :: (a -> a -> a) -> Maybe a -> a -> Maybe a
mfl _ Nothing x = Just x
mfl fun (Just y) x = Just (fun y x)

foldl1Note :: Foldable t => String -> (a -> a -> a) -> t a -> a
foldl1Note msg fun fld = fromMaybe (error $ "Safe.Foldable.foldl1Note: empty list, " ++ msg) (foldl (mfl fun) Nothing fld)

foldl1Def :: Foldable t => a -> (a -> a -> a) -> t a -> a
foldl1Def def fun fld = fromMaybe def (foldl (mfl fun) Nothing fld)

foldl1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldl1May fun = foldl (mfl fun) Nothing

{-# DEPRECATED foldl1Safe "Use @foldl f mempty@ instead." #-}
foldl1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldl1Safe fun = foldl fun mempty


mfr :: (a -> a -> a) -> a -> Maybe a -> Maybe a
mfr _ x Nothing = Just x
mfr fun x (Just y) = Just (fun x y)

foldr1Note :: Foldable t => String -> (a -> a -> a) -> t a -> a
foldr1Note msg fun fld = fromMaybe (error $ "Safe.Foldable.foldr1Note: empty list, " ++ msg) (foldr (mfr fun) Nothing fld)

foldr1Def :: Foldable t => a -> (a -> a -> a) -> t a -> a
foldr1Def def fun fld = fromMaybe def (foldr (mfr fun) Nothing fld)

foldr1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldr1May fun = foldr (mfr fun) Nothing

{-# DEPRECATED foldr1Safe "Use @foldr f mempty@ instead." #-}
foldr1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldr1Safe fun = foldr fun mempty

-- |
-- > findJust op = fromJust . find op
findJust :: Foldable t => (a -> Bool) -> t a -> a
findJust op fld = fromMaybe (error "Safe.Foldable.findJust, item not found") (find op fld)

findJustDef :: Foldable t => a -> (a -> Bool) -> t a -> a
findJustDef def op fld = fromMaybe def (find op fld)

findJustNote :: Foldable t => String -> (a -> Bool) -> t a -> a
findJustNote msg op fld = fromMaybe (error $ "Safe.Foldable.findJustNote: element not found, " ++ msg) (find op fld)

{-# DEPRECATED findJustSafe "Use @findJustDef mempty@ instead." #-}
findJustSafe :: (Monoid m, Foldable t) => (m -> Bool) -> t m -> m
findJustSafe = findJustDef mempty
