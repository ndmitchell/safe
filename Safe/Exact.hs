{- |
Provides functions that raise errors in corner cases instead of returning \"best effort\"
results, then provides wrappers like the "Safe" module. As some examples:

* @'takeExact' 3 [1,2]@ raises an error, in contrast to 'take' which would return
  just two elements.

* @'takeExact' (-1) [1,2]@ raises an error, in contrast to 'take' which would return
  no elements.

* @'zip' [1,2] [1]@ raises an error, in contrast to 'zip' which would only pair up the
  first element.

Note that the @May@ variants of these functions are /strict/ in at least the bit of the prefix
of the list required to spot errors. The standard and @Note@ versions are lazy, but throw
errors later in the process - they do not check upfront.
-}
module Safe.Exact(
    -- * New functions
    takeExact, dropExact, splitAtExact,
    -- * Safe wrappers
    takeExactMay, takeExactNote,
    dropExactMay, dropExactNote,
    splitAtExactMay, splitAtExactNote,
    ) where

import Control.Arrow


---------------------------------------------------------------------
-- HELPERS

addNote note fun msg = error $
    "Safe.Exact." ++ fun ++ ", " ++ msg ++ (if null note then "" else ", " ++ note)


---------------------------------------------------------------------
-- IMPLEMENTATIONS

{-# INLINE splitAtExact_ #-}
splitAtExact_ :: (String -> r) -> ([a] -> r) -> (a -> r -> r) -> Int -> [a] -> r
splitAtExact_ err nil cons o xs
    | o < 0 = err $ "index must not be negative, index=" ++ show o
    | otherwise = f o xs
    where
        f 0 xs = nil xs
        f i (x:xs) = x `cons` f (i-1) xs
        f i [] = err $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)


---------------------------------------------------------------------
-- WRAPPERS

-- |
-- > takeExact n xs =
-- >   | n >= 0 && n <= length xs = take n xs
-- >   | otherwise                = error "some message"
takeExact :: Int -> [a] -> [a]
takeExact = splitAtExact_ (addNote "" "takeExact") (const []) (:)

-- |
-- > dropExact n xs =
-- >   | n >= 0 && n <= length xs = drop n xs
-- >   | otherwise                = error "some message"
dropExact :: Int -> [a] -> [a]
dropExact = splitAtExact_ (addNote "" "dropExact") id (flip const)

-- |
-- > splitAtExact n xs =
-- >   | n >= 0 && n <= length xs = splitAt n xs
-- >   | otherwise                = error "some message"
splitAtExact :: Int -> [a] -> ([a], [a])
splitAtExact = splitAtExact_ (addNote "" "splitAtExact")
    (\x -> ([], x)) (\a b -> first (a:) b)

takeExactNote :: String -> Int -> [a] -> [a]
takeExactNote note = splitAtExact_ (addNote note "takeExactNote") (const []) (:)

takeExactMay :: Int -> [a] -> Maybe [a]
takeExactMay = splitAtExact_ (const Nothing) (const $ Just []) (\a -> fmap (a:))

dropExactNote :: String -> Int -> [a] -> [a]
dropExactNote note = splitAtExact_ (addNote note "dropExactNote") id (flip const)

dropExactMay :: Int -> [a] -> Maybe [a]
dropExactMay = splitAtExact_ (const Nothing) Just (flip const)

splitAtExactNote :: String -> Int -> [a] -> ([a], [a])
splitAtExactNote note = splitAtExact_ (addNote note "splitAtExactNote")
    (\x -> ([], x)) (\a b -> first (a:) b)

splitAtExactMay :: Int -> [a] -> Maybe ([a], [a])
splitAtExactMay = splitAtExact_ (const Nothing)
    (\x -> Just ([], x)) (\a b -> fmap (first (a:)) b)
