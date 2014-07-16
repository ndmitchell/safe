{- |
Provides functions that raise errors in corner cases instead of returning \"best effort\"
results, then provides wrappers like the "Safe" module. For example:

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
    zipExact, zipWithExact,
    -- * Safe wrappers
    takeExactMay, takeExactNote, takeExactDef,
    dropExactMay, dropExactNote, dropExactDef,
    splitAtExactMay, splitAtExactNote, splitAtExactDef,
    zipExactMay, zipExactNote, zipExactDef,
    zipWithExactMay, zipWithExactNote, zipWithExactDef,
    ) where

import Data.Maybe ( fromMaybe )
import Control.Arrow
import Safe.Util ( (.^), (.^^) )

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


{-# INLINE zipWithExact_ #-}
zipWithExact_ :: (String -> r) -> r -> (a -> b -> r -> r) -> [a] -> [b] -> r
zipWithExact_ err nil cons = f
    where
        f (x:xs) (y:ys) = cons x y $ f xs ys
        f [] [] = nil
        f [] _ = err "second list is longer than the first"
        f _ [] = err "first list is longer than the second"


---------------------------------------------------------------------
-- TAKE/DROP/SPLIT

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

takeExactDef :: [a] -> Int -> [a] -> [a]
takeExactDef def = fromMaybe def .^ takeExactMay

dropExactNote :: String -> Int -> [a] -> [a]
dropExactNote note = splitAtExact_ (addNote note "dropExactNote") id (flip const)

dropExactMay :: Int -> [a] -> Maybe [a]
dropExactMay = splitAtExact_ (const Nothing) Just (flip const)

dropExactDef :: [a] -> Int -> [a] -> [a]
dropExactDef def = fromMaybe def .^ dropExactMay

splitAtExactNote :: String -> Int -> [a] -> ([a], [a])
splitAtExactNote note = splitAtExact_ (addNote note "splitAtExactNote")
    (\x -> ([], x)) (\a b -> first (a:) b)

splitAtExactMay :: Int -> [a] -> Maybe ([a], [a])
splitAtExactMay = splitAtExact_ (const Nothing)
    (\x -> Just ([], x)) (\a b -> fmap (first (a:)) b)

splitAtExactDef :: ([a], [a]) -> Int -> [a] -> ([a], [a])
splitAtExactDef def = fromMaybe def .^ splitAtExactMay

---------------------------------------------------------------------
-- ZIP

-- |
-- > zipExact xs ys =
-- >   | length xs == length ys = zip xs ys
-- >   | otherwise              = error "some message"
zipExact :: [a] -> [b] -> [(a,b)]
zipExact = zipWithExact_ (addNote "" "zipExact") []  (\a b xs -> (a,b) : xs)

-- |
-- > zipWithExact f xs ys =
-- >   | length xs == length ys = zipWith f xs ys
-- >   | otherwise              = error "some message"
zipWithExact :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExact f = zipWithExact_ (addNote "" "zipWithExact") [] (\a b xs -> f a b : xs)


zipExactNote :: String -> [a] -> [b] -> [(a,b)]
zipExactNote note = zipWithExact_ (addNote note "zipExactNote") []  (\a b xs -> (a,b) : xs)

zipExactMay :: [a] -> [b] -> Maybe [(a,b)]
zipExactMay = zipWithExact_ (const Nothing) (Just [])  (\a b xs -> fmap ((a,b) :) xs)

zipExactDef :: [(a,b)] -> [a] -> [b] -> [(a,b)]
zipExactDef def = fromMaybe def .^ zipExactMay

zipWithExactNote :: String -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExactNote note f = zipWithExact_ (addNote note "zipWithExactNote") []  (\a b xs -> f a b : xs)

zipWithExactMay :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithExactMay f = zipWithExact_ (const Nothing) (Just [])  (\a b xs -> fmap (f a b :) xs)

zipWithExactDef :: [c] -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExactDef def = fromMaybe def .^^ zipWithExactMay
