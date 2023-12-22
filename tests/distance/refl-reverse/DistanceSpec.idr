module DistanceSpec

import Test.Common

reverse_reflexivity : XZ => Property
reverse_reflexivity = withTests 1000 $ property $ do
  (s1, s2) <- forAll bytesString2
  let d = ncd s1 s2
  if d == 0.0
    then s1 === s2
    else s1 /== s2

main : IO ()
main = testProperty reverse_reflexivity
