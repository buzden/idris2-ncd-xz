module DistanceSpec

import Test.Common

check_distribution : XZ => Property
check_distribution = verifiedTermination {min=300} . withTests 50000 . property $ do
  (s1, s2) <- forAll bytesString2
  let d = ncd s1 s2
  let class = the Nat $ cast $ d.asDouble * 10
  for_ [0..9] $ \n =>
    --classify (fromString $ show n) (class == n)
    cover 1 (fromString $ show n) (class == n)

main : IO ()
main = testProperty check_distribution
