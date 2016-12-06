import Data.Text.Arbitrary
import Types.Xandar
import Test.QuickCheck

instance Arbitrary User where
    arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = putStrLn "Test suite not yet implemented"