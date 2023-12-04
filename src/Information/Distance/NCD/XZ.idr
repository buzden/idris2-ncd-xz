module Information.Distance.NCD.XZ

import public Control.Monad.Error.Interface

import Data.FilePath.File

import public Information.Distance.NCD

%default total

--- XZ availability interface ---

public export
data XZFileError
  = FileNotFound AnyFile
  | Can'tRead AnyFile
  | Couldn'tCompress (List AnyFile)

export
Interpolation XZFileError where
  interpolate $ FileNotFound file   = "File not found: `\{file}`"
  interpolate $ Can'tRead file      = "Cannot read from `\{file}`"
  interpolate $ Couldn'tCompress fs = "Could not compress files \{fs}" where
    Interpolation a => Interpolation (List a) where
      interpolate = joinBy ", " . map interpolate

public export
interface XZ where
  xzStrLen  : List String -> Nat
  xzFileLen : HasIO m => MonadError XZFileError m => List AnyFile -> m Nat

--- Getting the XZ ---

public export
data XZUsabilityError
  = XZIsNotUsable
  | WCIsNotUsable
  | CatIsNotUsable

export
Interpolation XZUsabilityError where
  interpolate XZIsNotUsable  = "The `xz` command is not usable"
  interpolate WCIsNotUsable  = "The `wc` command is not usable"
  interpolate CatIsNotUsable = "The `cat` command is not usable"

export
externalXZ : HasIO m => MonadError XZUsabilityError m => m XZ
-- check that `xz`, `wc` and `cat` are usable
-- don't forget to escape the given file name with `System.Escape.escapeArg`; maybe, implement local `Interpolation AnyFile` for that

--- Implementations of NCD using `xz` ---

ncdValue : (a, b, ab, ba, aa, bb : Nat) -> DoubleBetween 0 1
ncdValue a b ab ba aa bb = ?divide_nicely (min ab ba `minus` min aa bb) (max a b)

export
XZ => NCD Prelude.id String where
  ncd sA sB = do
    let a  = xzStrLen [sA]
    let b  = xzStrLen [sB]
    let ab = xzStrLen [sA, sB]
    let ba = xzStrLen [sB, sA]
    let aa = xzStrLen [sA, sA]
    let bb = xzStrLen [sB, sB]
    ncdValue a b ab ba aa bb

export
XZ => HasIO m => MonadError XZFileError m => NCD m AnyFile where
  ncd sA sB = do
    let a  = xzFileLen [sA]
    let b  = xzFileLen [sB]
    let ab = xzFileLen [sA, sB]
    let ba = xzFileLen [sB, sA]
    let aa = xzFileLen [sA, sA]
    let bb = xzFileLen [sB, sB]
    [| ncdValue a b ab ba aa bb |]
