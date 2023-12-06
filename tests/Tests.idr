module Tests

import Test.Golden.RunnerHelper

main : IO ()
main = goldenRunner
  [ "Documentation" `atDir` "docs"
  , "Tests infrastructure" `atDir` "common/tests"
  , "Distance properties" `atDir` "distance"
  ]
