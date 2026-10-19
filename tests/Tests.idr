module Tests

import Test.Golden.RunnerHelper

main : IO ()
main = goldenRunner
  [ "Documentation" `atDir` "docs"
  , "Panhard measure" `atDir` "panhard"
  , "Tests infrastructure" `atDir` "common/tests"
  , "Distance properties" `atDir` "distance"
  , "File-based operations" `atDir` "files"
  ]
