
import System.Process.Extra
import Control.Exception.Extra

main :: IO ()
main = do
    retry 3 $ system_ "cabal install QuickCheck deepseq"
    system_ "runhaskell Test"
