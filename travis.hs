
import Neil

main :: IO ()
main = do
    retry 3 $ cmd "cabal install QuickCheck"
    cmd "runhaskell Test"
