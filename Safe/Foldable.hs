{- |
Versions of 'Foldable' functions, with wrappers like the "Safe" module.
-}
module Safe.Foldable(
    -- * New functions
    findJust,
    -- * Safe wrappers
    foldl1May, foldl1Def, foldl1Note,
    foldr1May, foldr1Def, foldr1Note,
    findJustDef, findJustNote,
    minimumMay, minimumDef, minimumNote,
    maximumMay, maximumDef, maximumNote,
    minimumByMay, minimumByDef, minimumByNote,
    maximumByMay, maximumByDef, maximumByNote,
    -- * Deprecated
    foldl1Safe, foldr1Safe, findJustSafe
    ) where

import Safe.Util
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Prelude hiding
    (foldl, foldl1, foldr, foldr1
    ,minimum, minimumBy, maximum, maximumBy)


---------------------------------------------------------------------
-- UTILITIES

fromNote = fromNoteModule "Safe.Foldable"

isNull :: Foldable t => t a -> Bool
isNull = null . toList


---------------------------------------------------------------------
-- WRAPPERS

foldl1May, foldr1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldl1May = liftMay isNull . foldl1
foldr1May = liftMay isNull . foldr1

foldl1Note, foldr1Note :: Foldable t => String -> (a -> a -> a) -> t a -> a
foldl1Note note = fromNote note "foldl1Note on empty" .^ foldl1May
foldr1Note note = fromNote note "foldr1Note on empty" .^ foldr1May

foldl1Def, foldr1Def :: Foldable t => a -> (a -> a -> a) -> t a -> a
foldl1Def def = fromMaybe def .^ foldl1May
foldr1Def def = fromMaybe def .^ foldr1May

minimumMay, maximumMay :: (Foldable t, Ord a) => t a -> Maybe a
minimumMay = liftMay isNull minimum
maximumMay = liftMay isNull maximum

minimumDef, maximumDef :: (Foldable t, Ord a) => a -> t a -> a
minimumDef def = fromMaybe def . minimumMay
maximumDef def = fromMaybe def . maximumMay

minimumNote, maximumNote :: (Foldable t, Ord a) => String -> t a -> a
minimumNote note = fromNote note "minimumNote on empty" . minimumMay
maximumNote note = fromNote note "maximumNote on empty" . maximumMay

minimumByMay, maximumByMay :: (Foldable t, Ord a) => (a -> a -> Ordering) -> t a -> Maybe a
minimumByMay = liftMay isNull . minimumBy
maximumByMay = liftMay isNull . maximumBy

minimumByDef, maximumByDef :: (Foldable t, Ord a) => a -> (a -> a -> Ordering) -> t a -> a
minimumByDef def = fromMaybe def .^ minimumByMay
maximumByDef def = fromMaybe def .^ maximumByMay

minimumByNote, maximumByNote :: (Foldable t, Ord a) => String -> (a -> a -> Ordering) -> t a -> a
minimumByNote note = fromNote note "minimumByNote on empty" .^ minimumByMay
maximumByNote note = fromNote note "maximumByNote on empty" .^ maximumByMay

-- |
-- > findJust op = fromJust . find op
findJust :: Foldable t => (a -> Bool) -> t a -> a
findJust = fromNote "" "findJust, no matching value" .^ find

findJustDef :: Foldable t => a -> (a -> Bool) -> t a -> a
findJustDef def = fromMaybe def .^ find

findJustNote :: Foldable t => String -> (a -> Bool) -> t a -> a
findJustNote note = fromNote note "findJustNote, no matching value" .^ find


---------------------------------------------------------------------
-- DEPRECATED

{-# DEPRECATED foldl1Safe "Use @foldl f mempty@ instead." #-}
foldl1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldl1Safe fun = foldl fun mempty

{-# DEPRECATED foldr1Safe "Use @foldr f mempty@ instead." #-}
foldr1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldr1Safe fun = foldr fun mempty


{-# DEPRECATED findJustSafe "Use @findJustDef mempty@ instead." #-}
findJustSafe :: (Monoid m, Foldable t) => (m -> Bool) -> t m -> m
findJustSafe = findJustDef mempty
