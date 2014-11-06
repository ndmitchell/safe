
import System.Process.Extra
import Control.Exception.Extra

main :: IO ()
main = do
    system "cabal install deepseq" -- will fail on GHC 7.4 and above, since already installed, which is fine
    retry 3 $ system_ "cabal install QuickCheck"
    system_ "runhaskell Test"
