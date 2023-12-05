module DistanceSpec

import Information.Distance.NCD.XZ

import Hedgehog

import System

bytesString : Gen String
bytesString = string (linear 0 10000) $ char $ constantFrom 'a' '\0' '\xFF'

direct_reflexivity : XZ => Property
direct_reflexivity = property $ do
  str <- forAll bytesString
  ncd str str === 0.0

main : IO ()
main = do
  Right _ <- externalXZ | Left e => die "Error during preparation of using XZ: \{e}"
  test
    [ "distance" `MkGroup`
      [ ("direct_reflexivity", direct_reflexivity)
      ]
    ]
