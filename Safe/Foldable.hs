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

import Safe.Util
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Prelude hiding (foldl, foldl1, foldr, foldr1)

---------------------------------------------------------------------
-- UTILITIES

fromNote = fromNoteModule "Data.Foldable"

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

-- |
-- > findJust op = fromJust . find op
findJust :: Foldable t => (a -> Bool) -> t a -> a
findJust = fromNote "" "findJust, no matching values" .^ find

findJustDef :: Foldable t => a -> (a -> Bool) -> t a -> a
findJustDef def = fromMaybe def .^ find

findJustNote :: Foldable t => String -> (a -> Bool) -> t a -> a
findJustNote note = fromNote note "findJustNote, no matching values" .^ find


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
