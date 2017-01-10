
-- | Internal utilities.
module Safe.Util where

import Data.Maybe


(.^) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.^) f g x1 x2 = f (g x1 x2)

(.^^) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(.^^) f g x1 x2 x3 = f (g x1 x2 x3)

(.^^^) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> c
(.^^^) f g x1 x2 x3 x4 = f (g x1 x2 x3 x4)

liftMay :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
liftMay test func val = if test val then Nothing else Just $ func val

fromNoteModule :: String -> String -> String -> Maybe a -> a
fromNoteModule modu note func = fromMaybe (error msg)
    where msg = modu ++ "." ++ func ++ (if null note then "" else ", " ++ note)

fromNoteEitherModule :: String -> String -> String -> Either String a -> a
fromNoteEitherModule modu note func = either (error . msg) id
    where msg ex = modu ++ "." ++ func ++ " " ++ ex ++ (if null note then "" else ", " ++ note)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
