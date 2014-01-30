{- |
A library for safe functions, based on standard functions that may crash.

In general, each unsafe function has up to 4 forms.
Since 'tail' has all the possible forms, it is fully documented.
The others all follow the same pattern.


* @Note@, takes an extra argument which supplements the error message, 'tailNote'

* @Def@, take an extra argument to give when a crash would otherwise happen, 'tailDef'

* @May@, wraps the result in a Maybe, 'tailMay'

* @Safe@, returns a default type if possible, 'tailSafe'

This library also introduces three brand new functions:

* 'at' - synonym for @(!!)@

* 'lookupJust' - defined as @lookupJust k = fromJust . lookup k@

* 'findJust' - defined as @findJust f = fromJust . find f@

* 'abort' - same as @error@, but different intended meaning

-}

module Safe(
    tailDef, tailMay, tailNote, tailSafe,
    initDef, initMay, initNote, initSafe,
    headDef, headMay, headNote,
    lastDef, lastMay, lastNote,
    minimumDef, minimumMay, minimumNote,
    maximumDef, maximumMay, maximumNote,
    foldr1Def, foldr1May, foldr1Note,
    foldl1Def, foldl1May, foldl1Note,
    foldl1Def', foldl1May', foldl1Note',
    fromJustDef, fromJustNote,
    assertNote,
    at, atDef, atMay, atNote,
    readDef, readMay, readNote,
    lookupJust, lookupJustDef, lookupJustNote,
    findJust, findJustDef, findJustNote,
    abort
    ) where


import Data.List
import Data.Maybe


liftDef :: (a -> b) -> (a -> Bool) -> b -> (a -> b)
liftDef func test def val = if test val then def else func val

liftMay :: (a -> b) -> (a -> Bool) -> (a -> Maybe b)
liftMay func test val = if test val then Nothing else Just $ func val

liftNote :: (a -> b) -> (a -> Bool) -> String -> String -> (a -> b)
liftNote func test caller note val =
    if test val
    then error $ "Pattern match failure, " ++ caller ++ ", " ++ note
    else func val

liftSafe :: (a -> a) -> (a -> Bool) -> (a -> a)
liftSafe func test val = if test val then val else func val


-- |
-- > tailDef [12] [] = [12]
-- > tailDef [12] [1,3,4] = [3,4]
tailDef  :: [a] -> [a] -> [a]
tailDef  = liftDef  tail null

-- |
-- > tailMay [] = Nothing
-- > tailMay [1,3,4] = Just [3,4]
tailMay  :: [a] -> Maybe [a]
tailMay  = liftMay  tail null

-- |
-- > tail "help me" [] = error "Pattern match failure, tail [], help me"
-- > tail "help me" [1,3,4] = [3,4]
tailNote :: String -> [a] -> [a]
tailNote = liftNote tail null "tail []"

-- |
-- > tailSafe [] = []
-- > tailSafe [1,3,4] = [3,4]
tailSafe :: [a] -> [a]
tailSafe = liftSafe tail null


initDef  :: [a] -> [a] -> [a]
initDef  = liftDef  init null

initMay  :: [a] -> Maybe [a]
initMay  = liftMay  init null

initNote :: String -> [a] -> [a]
initNote = liftNote init null "init []"

initSafe :: [a] -> [a]
initSafe = liftSafe init null



headDef  :: a -> [a] -> a
headDef  = liftDef  head null

headMay  :: [a] -> Maybe a
headMay  = liftMay  head null

headNote :: String -> [a] -> a
headNote = liftNote head null "head []"


lastDef  :: a -> [a] -> a
lastDef  = liftDef  last null

lastMay  :: [a] -> Maybe a
lastMay  = liftMay  last null

lastNote :: String -> [a] -> a
lastNote = liftNote last null "last []"



minimumDef  :: Ord a => a -> [a] -> a
minimumDef  = liftDef  minimum null

minimumMay  :: Ord a => [a] -> Maybe a
minimumMay  = liftMay  minimum null

minimumNote :: Ord a => String -> [a] -> a
minimumNote = liftNote minimum null "minimum []"


maximumDef  :: Ord a => a -> [a] -> a
maximumDef  = liftDef  maximum null

maximumMay  :: Ord a => [a] -> Maybe a
maximumMay  = liftMay  maximum null

maximumNote :: Ord a => String -> [a] -> a
maximumNote = liftNote maximum null "maximum []"



foldr1Def  :: a -> (a -> a -> a) -> [a] -> a
foldr1Def def f = liftDef (foldr1 f) null def

foldr1May  :: (a -> a -> a) -> [a] -> Maybe a
foldr1May f = liftMay (foldr1 f) null

foldr1Note :: String -> (a -> a -> a) -> [a] -> a
foldr1Note note f = liftNote (foldr1 f) null "foldr1 []" note


foldl1Def  :: a -> (a -> a -> a) -> [a] -> a
foldl1Def def f = liftDef (foldl1 f) null def

foldl1May  :: (a -> a -> a) -> [a] -> Maybe a
foldl1May f = liftMay (foldl1 f) null

foldl1Note :: String -> (a -> a -> a) -> [a] -> a
foldl1Note note f = liftNote (foldl1 f) null "foldl1 []" note


foldl1Def'  :: a -> (a -> a -> a) -> [a] -> a
foldl1Def' def f = liftDef (foldl1' f) null def

foldl1May'  :: (a -> a -> a) -> [a] -> Maybe a
foldl1May' f = liftMay (foldl1' f) null

foldl1Note' :: String -> (a -> a -> a) -> [a] -> a
foldl1Note' note f = liftNote (foldl1' f) null "foldl1' []" note


-- | See fromMaybe
fromJustDef :: a -> Maybe a -> a
fromJustDef  = liftDef fromJust isNothing

fromJustNote :: String -> Maybe a -> a
fromJustNote = liftNote fromJust isNothing "fromJust Nothing"



assertNote :: String -> Bool -> a -> a
assertNote msg False val = error $ "assertion failed, " ++ msg
assertNote msg True  val = val



-- | Same as @(!!)@, but better error message
at :: [a] -> Int -> a
at = atNote "called by at"

atDef :: a -> [a] -> Int -> a
atDef def x n = fromMaybe def (atMay x n)

atMay :: [a] -> Int -> Maybe a
atMay xs n | n < 0 = Nothing
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs (n-1)

atNote :: String -> [a] -> Int -> a
atNote msg _ n | n < 0 = error $ "Safe.at: negative index, " ++ msg
atNote msg xs n = f xs n
    where
        f [] i = error $ "Safe.at: index too large, index=" ++ show n ++ ", length=" ++ show (n-i) ++ ", " ++ msg
        f (x:_) 0 = x
        f (_:xs) i = f xs (i-1)



readDef :: Read a => a -> String -> a
readDef def s = fromMaybe def (readMay s)

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing

readNote :: Read a => String -> String -> a
readNote msg s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                     [x] -> x
                     []  -> error $ "Prelude.read: no parse, " ++ msg ++ ", on " ++ prefix
                     _   -> error $ "Prelude.read: ambiguous parse, " ++ msg ++ ", on " ++ prefix
    where
        maxLength = 15
        prefix = '\"' : a ++ if length s <= maxLength then (b ++ "\"") else "...\""
            where (a,b) = splitAt (maxLength - 3) s


-- |
-- > lookupJust key = fromJust . lookup key
lookupJust :: Eq a => a -> [(a,b)] -> b
lookupJust key = fromJustNote "lookupJust, item not found" . lookup key

lookupJustDef :: Eq a => b -> a -> [(a,b)] -> b
lookupJustDef def key lst = fromMaybe def (lookup key lst)

lookupJustNote :: Eq a => String -> a -> [(a,b)] -> b
lookupJustNote msg key lst = case lookup key lst of
                                 Nothing -> error $ "Safe.lookupJust: element not found, " ++ msg
                                 Just x -> x



-- |
-- > findJust op = fromJust . find op
findJust :: (a -> Bool) -> [a] -> a
findJust op = fromJustNote "findJust, item not found" . find op

findJustDef :: a -> (a -> Bool) -> [a] -> a
findJustDef def op lst = fromMaybe def (find op lst)

findJustNote :: String -> (a -> Bool) -> [a] -> a
findJustNote msg op lst = case find op lst of
                               Nothing -> error $ "Safe.findJust: element not found, " ++ msg
                               Just x -> x



-- | Exactly the same as @error@. Use this for instances where the program
--   has decided to exit because of invalid user input, or the user pressed
--   quit etc. This allows @error@ to be reserved for genuine coding mistakes.
abort :: String -> a
abort = error
