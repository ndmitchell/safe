{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import Safe
import Safe.Exact
import qualified Safe.Foldable as F

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.IO.Unsafe
import Test.QuickCheck hiding ((===))


---------------------------------------------------------------------
-- TESTS

main :: IO ()
main = do
    -- All from the docs, so check they match
    tailMay dNil === Nothing
    tailMay [1,3,4] === Just [3,4]
    tailDef [12] [] === [12]
    tailDef [12] [1,3,4] === [3,4]
    tailNote "help me" dNil `err` "Safe.tailNote [], help me"
    tailNote "help me" [1,3,4] === [3,4]
    tailSafe [] === dNil
    tailSafe [1,3,4] === [3,4]

    findJust (== 2) [d1,2,3] === 2
    findJust (== 4) [d1,2,3] `err` "Safe.findJust"
    F.findJust (== 2) [d1,2,3] === 2
    F.findJust (== 4) [d1,2,3] `err` "Safe.Foldable.findJust"
    F.findJustDef 20 (== 4) [d1,2,3] === 20
    F.findJustNote "my note" (== 4) [d1,2,3] `errs` ["Safe.Foldable.findJustNote","my note"]

    takeExact 3 [d1,2] `errs` ["Safe.Exact.takeExact","index=3","length=2"]
    takeExact (-1) [d1,2] `errs` ["Safe.Exact.takeExact","negative","index=-1"]
    takeExact 1 (takeExact 3 [d1,2]) === [1] -- test is lazy

    quickCheck $ \(Int10 i) (List10 (xs :: [Int])) -> do
        let (t,d) = splitAt i xs
        let good = length t == i
        let f name exact may note res =
                if good then do
                    exact i xs === res
                    note "foo" i xs === res
                    may i xs === Just res
                else do
                    exact i xs `err` ("Safe.Exact." ++ name ++ "Exact")
                    note "foo" i xs `errs` ["Safe.Exact." ++ name ++ "ExactNote","foo"]
                    may i xs === Nothing
        f "take" takeExact takeExactMay takeExactNote t
        f "drop" dropExact dropExactMay dropExactNote d
        f "splitAt" splitAtExact splitAtExactMay splitAtExactNote (t, d)

    take 2 (zipExact [1,2,3] [1,2]) === [(1,1),(2,2)]
    zipExact [d1,2,3] [d1,2] `errs` ["Safe.Exact.zipExact","first list is longer than the second"]
    zipExact [d1,2] [d1,2,3] `errs` ["Safe.Exact.zipExact","second list is longer than the first"]
    zipExact dNil dNil === []

    quickCheck $ \(List10 (xs :: [Int])) x -> do
        let ys = maybeToList x ++ xs
        let res = zip xs ys
        let f name exact may note =
                if isNothing x then do
                    exact xs ys === res
                    note "foo" xs ys === res
                    may xs ys === Just res
                else do
                    exact xs ys `err` ("Safe.Exact." ++ name ++ "Exact")
                    note "foo" xs ys `errs` ["Safe.Exact." ++ name ++ "ExactNote","foo"]
                    may xs ys === Nothing
        f "zip" zipExact zipExactMay zipExactNote
        f "zipWith" (zipWithExact (,)) (zipWithExactMay (,)) (flip zipWithExactNote (,))


---------------------------------------------------------------------
-- UTILITIES

d1 = 1 :: Double
dNil = [] :: [Double]

(===) :: (Show a, Eq a) => a -> a -> IO ()
(===) a b = when (a /= b) $ error $ "Mismatch: " ++ show a ++ " /= " ++ show b

err :: NFData a => a -> String -> IO ()
err a b = errs a [b]

errs :: NFData a => a -> [String] -> IO ()
errs a bs = do
    res <- try $ evaluate $ rnf a
    case res of
        Right v -> error $ "Expected error, but succeeded: " ++ show bs
        Left (msg :: SomeException) -> forM_ bs $ \b -> do
            let s = show msg
            unless (b `isInfixOf` s) $ error $ "Invalid error string, got " ++ show s ++ ", want " ++ show b
            let f xs = " " ++ map (\x -> if sepChar x then ' ' else x) xs ++ " "
            unless (f b `isInfixOf` f s) $ error $ "Not standalone error string, got " ++ show s ++ ", want " ++ show b

sepChar x = x `elem` " ,;."

newtype Int10 = Int10 Int deriving Show

instance Arbitrary Int10 where
    arbitrary = fmap Int10 $ choose (-3, 10)

newtype List10 a = List10 [a] deriving Show

instance Arbitrary a => Arbitrary (List10 a) where
    arbitrary = do i <- choose (0, 10); fmap List10 $ vector i

instance Testable () where
    property () = property True

instance Testable a => Testable (IO a) where
    property = property . unsafePerformIO
