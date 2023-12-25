module DistanceSpec

import Test.Common

check_distribution_2 : XZ => Property
check_distribution_2 = verifiedTermination {min=300} . withConfidence 10000000 . withTests 50000 . property $ do
  (s1, s2) <- forAll bytesString2
  let d = ncd s1 s2
  let class = the Nat $ cast $ d.asDouble * 10
  for_ [0..9] $ \n =>
    --classify (fromString $ show n) (class == n)
    cover 1 (fromString $ show n) (class == n)

main : IO ()
main = testProperty check_distribution_2
