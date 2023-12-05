module DistanceSpec

import Test.Common

direct_reflexivity : XZ => Property
direct_reflexivity = property $ do
  str <- forAll bytesString
  ncd str str === 0.0

main : IO ()
main = testProperty direct_reflexivity
