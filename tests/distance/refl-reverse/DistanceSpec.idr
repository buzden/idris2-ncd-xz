module DistanceSpec

import Test.Common

reverse_reflexivity : XZ => Property
reverse_reflexivity = property $ do
  (s1, s2) <- forAll bytesString2
  let d = ncd s1 s2
  if s1 == s2
    then d === 0.0
    else diff d (>) 0.0

main : IO ()
main = testProperty reverse_reflexivity
