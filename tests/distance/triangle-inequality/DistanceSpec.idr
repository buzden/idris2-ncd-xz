module DistanceSpec

import Test.Common

triangle_inequality : XZ => Property
triangle_inequality = withTests 1000 $ property $ do
  [s1, s2, s3] <- forAll $ bytesStrings {n=2}
  let n12 = ncd s1 s2
  let n23 = ncd s2 s3
  let n13 = ncd s1 s3
  let n123 : DoubleBetween _ _ := n12 + n23
  annotate "AB = \{show n12}, BA = \{show n23}"
  annotate "AB+BC = \{show n123}, AC = \{show n13}"
  diff (weakenBounds n13) (<=) n123

main : IO ()
main = testProperty triangle_inequality
