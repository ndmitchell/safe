{- |
A module wrapping @Prelude@/@Data.List@ functions that can throw exceptions, such as @head@ and @!!@.
Each unsafe function has up to four variants, e.g. with @tail@:

* @'tail' :: [a] -> [a]@, raises an error on @tail []@.

* @'tailMay' :: [a] -> /Maybe/ [a]@, turns errors into @Nothing@.

* @'tailDef' :: /[a]/ -> [a] -> [a]@, takes a default to return on errors.

* @'tailNote' :: /String/ -> [a] -> [a]@, takes an extra argument which supplements the error message.

* @'tailSafe' :: [a] -> [a]@, returns some sensible default if possible, @[]@ in the case of @tail@.

This module also introduces some new functions, documented at the top of the module.
-}

module Safe(
    -- * New functions
    abort, at, lookupJust, findJust, elemIndexJust, findIndexJust,
    -- * Safe wrappers
    tailMay, tailDef, tailNote, tailSafe,
    initMay, initDef, initNote, initSafe,
    headMay, headDef, headNote,
    lastMay, lastDef, lastNote,
    minimumMay, minimumDef, minimumNote,
    maximumMay, maximumDef, maximumNote,
    minimumByMay, minimumByDef, minimumByNote,
    maximumByMay, maximumByDef, maximumByNote,
    foldr1May, foldr1Def, foldr1Note,
    foldl1May, foldl1Def, foldl1Note,
    foldl1May', foldl1Def', foldl1Note',
    scanl1May, scanl1Def, scanl1Note,
    scanr1May, scanr1Def, scanr1Note,
    cycleMay, cycleDef, cycleNote,
    fromJustDef, fromJustNote,
    assertNote,
    atMay, atDef, atNote,
    readMay, readDef, readNote, readEitherSafe,
    lookupJustDef, lookupJustNote,
    findJustDef, findJustNote,
    elemIndexJustDef, elemIndexJustNote,
    findIndexJustDef, findIndexJustNote,
    toEnumMay, toEnumDef, toEnumNote, toEnumSafe,
    succMay, succDef, succNote, succSafe,
    predMay, predDef, predNote, predSafe,
    ) where

import Safe.Util
import Data.List
import Data.Maybe
import Safe.Partial

---------------------------------------------------------------------
-- UTILITIES

fromNote :: Partial => String -> String -> Maybe a -> a
fromNote = fromNoteModule "Safe"

fromNoteEither :: Partial => String -> String -> Either String a -> a
fromNoteEither = fromNoteEitherModule "Safe"


---------------------------------------------------------------------
-- IMPLEMENTATIONS

-- | Synonym for 'error'. Used for instances where the program
--   has decided to exit because of invalid user input, or the user pressed
--   quit etc. This function allows 'error' to be reserved for programmer errors.
abort :: Partial => String -> a
abort = error


at_ :: [a] -> Int -> Either String a
at_ xs o | o < 0 = Left $ "index must not be negative, index=" ++ show o
         | otherwise = f o xs
    where f 0 (x:xs) = Right x
          f i (x:xs) = f (i-1) xs
          f i [] = Left $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)


---------------------------------------------------------------------
-- WRAPPERS

-- |
-- > tailMay [] = Nothing
-- > tailMay [1,3,4] = Just [3,4]
tailMay :: [a] -> Maybe [a]
tailMay = liftMay null tail

-- |
-- > tailDef [12] [] = [12]
-- > tailDef [12] [1,3,4] = [3,4]
tailDef :: [a] -> [a] -> [a]
tailDef def = fromMaybe def . tailMay

-- |
-- > tailNote "help me" [] = error "Safe.tailNote [], help me"
-- > tailNote "help me" [1,3,4] = [3,4]
tailNote :: Partial => String -> [a] -> [a]
tailNote note = fromNote note "tailNote []" . tailMay

-- |
-- > tailSafe [] = []
-- > tailSafe [1,3,4] = [3,4]
tailSafe :: [a] -> [a]
tailSafe = tailDef []


initMay :: [a] -> Maybe [a]
initMay = liftMay null init

initDef :: [a] -> [a] -> [a]
initDef def = fromMaybe def . initMay

initNote :: Partial => String -> [a] -> [a]
initNote note = fromNote note "initNote []" . initMay

initSafe :: [a] -> [a]
initSafe = initDef []



headMay, lastMay :: [a] -> Maybe a
headMay = liftMay null head
lastMay = liftMay null last

headDef, lastDef :: a -> [a] -> a
headDef def = fromMaybe def . headMay
lastDef def = fromMaybe def . lastMay

headNote, lastNote :: String -> [a] -> a
headNote note = fromNote note "headNote []" . headMay
lastNote note = fromNote note "lastNote []" . lastMay

minimumMay, maximumMay :: Ord a => [a] -> Maybe a
minimumMay = liftMay null minimum
maximumMay = liftMay null maximum

minimumDef, maximumDef :: Ord a => a -> [a] -> a
minimumDef def = fromMaybe def . minimumMay
maximumDef def = fromMaybe def . maximumMay

minimumNote, maximumNote :: (Partial, Ord a) => String -> [a] -> a
minimumNote note = fromNote note "minumumNote []" . minimumMay
maximumNote note = fromNote note "maximumNote []" . maximumMay

minimumByMay, maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay = liftMay null . minimumBy
maximumByMay = liftMay null . maximumBy

minimumByDef, maximumByDef :: a -> (a -> a -> Ordering) -> [a] -> a
minimumByDef def = fromMaybe def .^ minimumByMay
maximumByDef def = fromMaybe def .^ maximumByMay

minimumByNote, maximumByNote :: Partial => String -> (a -> a -> Ordering) -> [a] -> a
minimumByNote note = fromNote note "minumumByNote []" .^ minimumByMay
maximumByNote note = fromNote note "maximumByNote []" .^ maximumByMay


foldr1May, foldl1May, foldl1May' :: (a -> a -> a) -> [a] -> Maybe a
foldr1May = liftMay null . foldr1
foldl1May = liftMay null . foldl1
foldl1May' = liftMay null . foldl1'

foldr1Def, foldl1Def, foldl1Def' :: a -> (a -> a -> a) -> [a] -> a
foldr1Def def = fromMaybe def .^ foldr1May
foldl1Def def = fromMaybe def .^ foldl1May
foldl1Def' def = fromMaybe def .^ foldl1May'

foldr1Note, foldl1Note, foldl1Note' :: Partial => String -> (a -> a -> a) -> [a] -> a
foldr1Note note = fromNote note "foldr1Note []" .^ foldr1May
foldl1Note note = fromNote note "foldl1Note []" .^ foldl1May
foldl1Note' note = fromNote note "foldl1Note []" .^ foldl1May'

scanr1May, scanl1May :: (a -> a -> a) -> [a] -> Maybe [a]
scanr1May = liftMay null . scanr1
scanl1May = liftMay null . scanl1

scanr1Def, scanl1Def :: [a] -> (a -> a -> a) -> [a] -> [a]
scanr1Def def = fromMaybe def .^ scanr1May
scanl1Def def = fromMaybe def .^ scanl1May

scanr1Note, scanl1Note :: Partial => String -> (a -> a -> a) -> [a] -> [a]
scanr1Note note = fromNote note "scanr1Note []" .^ scanr1May
scanl1Note note = fromNote note "scanl1Note []" .^ scanl1May

cycleMay :: [a] -> Maybe [a]
cycleMay = liftMay null cycle

cycleDef :: [a] -> [a] -> [a]
cycleDef def = fromMaybe def . cycleMay

cycleNote :: Partial => String -> [a] -> [a]
cycleNote note = fromNote note "cycleNote []" . cycleMay

-- | An alternative name for 'fromMaybe', to fit the naming scheme of this package.
--   Generally using 'fromMaybe' directly would be considered better style.
fromJustDef :: a -> Maybe a -> a
fromJustDef  = fromMaybe

fromJustNote :: Partial => String -> Maybe a -> a
fromJustNote note = fromNote note "fromJustNote Nothing"

assertNote :: Partial => String -> Bool -> a -> a
assertNote note True val = val
assertNote note False val = fromNote note "assertNote False" Nothing


-- | Synonym for '!!', but includes more information in the error message.
at :: Partial => [a] -> Int -> a
at = fromNoteEither "" "at" .^ at_

atMay :: [a] -> Int -> Maybe a
atMay = eitherToMaybe .^ at_

atDef :: a -> [a] -> Int -> a
atDef def = fromMaybe def .^ atMay

atNote :: Partial => String -> [a] -> Int -> a
atNote note = fromNoteEither note "atNote" .^ at_

-- | This function provides a more precise error message than 'readEither' from 'base'.
readEitherSafe :: Read a => String -> Either String a
readEitherSafe s = case [x | (x,t) <- reads s, ("","") <- lex t] of
        [x] -> Right x
        []  -> Left $ "no parse on " ++ prefix
        _   -> Left $ "ambiguous parse on " ++ prefix
    where
        maxLength = 15
        prefix = '\"' : a ++ if length s <= maxLength then b ++ "\"" else "...\""
            where (a,b) = splitAt (maxLength - 3) s

readMay :: Read a => String -> Maybe a
readMay = eitherToMaybe . readEitherSafe

readDef :: Read a => a -> String -> a
readDef def = fromMaybe def . readMay

-- | 'readNote' uses 'readEitherSafe' for the error message.
readNote :: (Partial, Read a) => String -> String -> a
readNote note = fromNoteEither note "readNote" . readEitherSafe

-- |
-- > lookupJust key = fromJust . lookup key
lookupJust :: Eq a => a -> [(a,b)] -> b
lookupJust = fromNote "" "lookupJust, no matching value" .^ lookup

lookupJustDef :: Eq a => b -> a -> [(a,b)] -> b
lookupJustDef def = fromMaybe def .^ lookup

lookupJustNote :: (Partial, Eq a) => String -> a -> [(a,b)] -> b
lookupJustNote note = fromNote note "lookupJustNote, no matching value" .^ lookup

-- |
-- > findJust op = fromJust . find op
findJust :: (a -> Bool) -> [a] -> a
findJust = fromNote "" "findJust, no matching value" .^ find

findJustDef :: a -> (a -> Bool) -> [a] -> a
findJustDef def = fromMaybe def .^ find

findJustNote :: Partial => String -> (a -> Bool) -> [a] -> a
findJustNote note = fromNote note "findJustNote, no matching value" .^ find

-- |
-- > elemIndexJust op = fromJust . elemIndex op
elemIndexJust :: Eq a => a -> [a] -> Int
elemIndexJust = fromNote "" "elemIndexJust, no matching value" .^ elemIndex

elemIndexJustDef :: Eq a => Int -> a -> [a] -> Int
elemIndexJustDef def = fromMaybe def .^ elemIndex

elemIndexJustNote :: (Partial, Eq a) => String -> a -> [a] -> Int
elemIndexJustNote note = fromNote note "elemIndexJustNote, no matching value" .^ elemIndex

-- |
-- > findIndexJust op = fromJust . findIndex op
findIndexJust :: (a -> Bool) -> [a] -> Int
findIndexJust = fromNote "" "findIndexJust, no matching value" .^ findIndex

findIndexJustDef :: Int -> (a -> Bool) -> [a] -> Int
findIndexJustDef def = fromMaybe def .^ findIndex

findIndexJustNote :: Partial => String -> (a -> Bool) -> [a] -> Int
findIndexJustNote note = fromNote note "findIndexJustNote, no matching value" .^ findIndex

-- From http://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum
-- answer by C. A. McCann
toEnumMay :: (Enum a, Bounded a) => Int -> Maybe a
toEnumMay i =
  let r = toEnum i
      max = maxBound `asTypeOf` r
      min = minBound `asTypeOf` r
  in if i >= fromEnum min && i <= fromEnum max
  then Just r
  else Nothing

toEnumDef :: (Enum a, Bounded a) => a -> Int -> a
toEnumDef def = fromMaybe def . toEnumMay

toEnumNote :: (Partial, Enum a, Bounded a) => String -> Int -> a
toEnumNote note = fromNote note "toEnumNote, out of range" . toEnumMay

toEnumSafe :: (Enum a, Bounded a) => Int -> a
toEnumSafe = toEnumDef minBound

succMay :: (Enum a, Eq a, Bounded a) => a -> Maybe a
succMay = liftMay (== maxBound) succ

succDef :: (Enum a, Eq a, Bounded a) => a -> a -> a
succDef def = fromMaybe def . succMay

succNote :: (Partial, Enum a, Eq a, Bounded a) => String -> a -> a
succNote note = fromNote note "succNote, out of range" . succMay

succSafe :: (Enum a, Eq a, Bounded a) => a -> a
succSafe = succDef maxBound

predMay :: (Enum a, Eq a, Bounded a) => a -> Maybe a
predMay = liftMay (== minBound) pred

predDef :: (Enum a, Eq a, Bounded a) => a -> a -> a
predDef def = fromMaybe def . predMay

predNote :: (Partial, Enum a, Eq a, Bounded a) => String -> a -> a
predNote note = fromNote note "predNote, out of range" . predMay

predSafe :: (Enum a, Eq a, Bounded a) => a -> a
predSafe = predDef minBound
