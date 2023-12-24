module DistanceSpec

import Data.Vect
import Data.SortedSet

import Test.Common

check_distribution : XZ => Property
check_distribution = verifiedTermination {min=300} . withTests 50000 . property $ do
  n <- forAll $ nat $ linear 1 5
  ss <- map toList $ forAll $ bytesStrings {n}
  let classes = fromList $ [ncd s1 s2 | s1 <- ss, s2 <- ss, s1 /= s2] <&> \d => cast $ d.asDouble * 10
  for_ [0..9] $ \n =>
    cover 1 (fromString $ show n) (contains n classes)

main : IO ()
main = testProperty check_distribution
