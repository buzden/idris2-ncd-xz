module DistanceSpec

import Control.Monad.Either

import Data.FilePath.File

import System.File.ReadWrite

import Test.Common

files_as_strings : XZ => Property
files_as_strings = withTests 300 $ property $ do
  (s1, s2) <- forAll bytesString2
  let d = ncd s1 s2

  -- TODO to think how these files can be put in a temporary space, like `/tmp`
  let f1 = AF "one"
  let f2 = AF "another"
  for_ (with Prelude.Nil [(f1, s1), (f2, s2)]) $ \(f, s) => unsafePerformIO $
    writeFile (interpolate f) s <&>
      either (\err => failWith Nothing "Can't prepare file \{f}: \{show err}") pure
  let Right d' = unsafePerformIO $ runEitherT $ ncd f1 f2 {m=EitherT XZFileError IO}
    | Left err => failWith Nothing "Error during file-based NCD: \{err}"

  d === d'

  for_ (with Prelude.Nil [f1, f2]) $ \f => unsafePerformIO $
    removeFile (interpolate f) <&>
      either (\err => failWith Nothing "Can't delete file \{f}: \{show err}") pure

main : IO ()
main = testProperty files_as_strings
