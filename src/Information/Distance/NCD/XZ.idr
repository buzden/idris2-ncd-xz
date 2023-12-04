module Information.Distance.NCD.XZ

import public Control.Monad.Error.Interface

import Data.FilePath.File

import public Information.Distance.NCD

import System.File

%default total

--- XZ availability interface ---

public export
data XZFileError
  = FileNotFound AnyFile
  | Can'tRead AnyFile
  | Couldn'tCompress (List AnyFile)
  | Couldn'tReadBytesCount
  | BadBytesCount String

export
Interpolation XZFileError where
  interpolate $ FileNotFound file    = "File not found: `\{file}`"
  interpolate $ Can'tRead file       = "Cannot read from `\{file}`"
  interpolate $ Couldn'tCompress fs  = "Could not compress files \{fs}" where
    Interpolation a => Interpolation (List a) where
      interpolate = joinBy ", " . map interpolate
  interpolate Couldn'tReadBytesCount = "Could not read bytes count"
  interpolate $ BadBytesCount str    = "Bad bytes count: \{str}"

public export
interface XZ where
  xzStrLen  : List String -> Nat
  xzFileLen : HasIO m => MonadError XZFileError m => List AnyFile -> m Nat

--- Getting the XZ ---

namespace GettingXZ

  export
  data XZCmd = Xz | Wc | Cat

  public export
  data XZUsabilityError
    = CmdIsNotUsable XZCmd
    | CmdCan'tRead XZCmd
    | Can'tReadFromCmd XZCmd
    | CmdWritesWrong XZCmd String

  Interpolation XZCmd where
    interpolate Xz = "xz --format=raw --compress --stdout --no-warn --quiet"
    interpolate Wc = "wc -c"
    interpolate Cat = "cat"

  [CmdName] Interpolation XZCmd where
    interpolate Xz  = "xz"
    interpolate Wc  = "wc"
    interpolate Cat = "cat"

  export
  Interpolation XZUsabilityError where
    interpolate $ CmdIsNotUsable   cmd = let _ = CmdName in "The `\{cmd}` command is not usable"
    interpolate $ CmdCan'tRead     cmd = let _ = CmdName in "The `\{cmd}` command can't read input"
    interpolate $ Can'tReadFromCmd cmd = let _ = CmdName in "Cannot read output of the `\{cmd}` command"
    interpolate $ CmdWritesWrong cmd s = let _ = CmdName in "The `\{cmd}` writes unexpected output: \{s}"

  export
  externalXZ : HasIO n => MonadError XZUsabilityError n => n XZ
  externalXZ = assert_total $ do
    checkUsable Xz "abcd" $ \s => let l = length s in 4 <= l && l <= 10
    checkUsable Wc "abcd" $ (== 4) . length
    checkUsable Cat {arg="-"} "abcd" (== "abcd")
    pure CallExternXZ

    where
      covering
      checkUsable : (cmd : XZCmd) -> {default "" arg : String} -> (stdin : String) -> (stdoutCheck : String -> Bool) -> n ()
      checkUsable cmd stdin stdoutCheck = do
        Right subp <- popen2 "\{cmd} \{arg}" | _ => throwError $ CmdIsNotUsable cmd
        Right () <- fPutStr subp.input stdin | _ => throwError $ CmdCan'tRead cmd
        closeFile subp.input
        Right stdout <- fRead subp.output    | _ => throwError $ Can'tReadFromCmd cmd
        closeFile subp.output
        let True = stdoutCheck stdout        | _ => throwError $ CmdWritesWrong cmd stdout
        pure ()

      [CallExternXZ] XZ where

        xzStrLen ss = do
          let act = do
            Right subp <- popen2 "\{Xz} | \{Wc}"              | _ => pure Nothing
            Right _ <- for @{Compose} ss $ fPutStr subp.input | _ => pure Nothing
            closeFile subp.input
            Right n <- assert_total fRead subp.output         | _ => pure Nothing
            closeFile subp.output
            pure $ parsePositive n
          let _ = Monoid.Additive
          fromMaybe (foldMap length ss) $ unsafePerformIO act

        xzFileLen fs = do
          let cmd = "\{Cat} \{joinBy " " $ escapeArg . interpolate <$> fs} | \{Xz} | \{Wc}"
          Right f <- popen cmd Read        | _ => throwError $ Couldn'tCompress fs
          Right n <- assert_total fRead f  | _ => throwError $ Couldn'tReadBytesCount
          0 <- pclose f                    | _ => throwError $ Couldn'tCompress fs
          let Just n = parsePositive n     | _ => throwError $ BadBytesCount n
          pure n

--- Implementations of NCD using `xz` ---

ncdValue : (a, b, ab, ba, aa, bb : Nat) -> DoubleBetween 0 1
ncdValue a b ab ba aa bb = roughlyFit $ cast (min ab ba `minus` min aa bb) / cast (max a b)

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
