import Test.Hspec
import qualified PropSpec
import qualified CNFSpec

main :: IO ()
main = do
        hspec PropSpec.spec
        hspec CNFSpec.spec

