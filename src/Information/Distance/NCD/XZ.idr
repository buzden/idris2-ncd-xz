module Information.Distance.NCD.XZ

import public Control.Monad.Error.Interface
import Control.Monad.Error.Either

import Data.FilePath.File

import public Information.Distance.NCD

import System.File

%default total

--- XZ availability interface ---

public export
data XZFileError
  = FileIOError AnyFile FileError
  | Couldn'tCompress (List AnyFile)
  | Couldn'tReadBytesCount
  | BadBytesCount String

export
Interpolation XZFileError where
  interpolate $ FileIOError file err = "Problem with file `\{file}`: \{show err}"
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
    | CmdExitsWrong XZCmd Int

  Interpolation XZCmd where
    interpolate Xz  = "xz --format=raw --compress --stdout --no-warn --quiet"
    interpolate Wc  = "wc -c"
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
    interpolate $ CmdWritesWrong cmd s = let _ = CmdName in "The `\{cmd}` writes unexpected output: \"\{s}\""
    interpolate $ CmdExitsWrong cmd ec = let _ = CmdName in "The `\{cmd}` exits with unexpected exit code: \{show ec}"

  withPopen2 : HasIO io =>
               (cmd : String) ->
               (error : FileError -> io a) ->
               (finalise : (exitCode : Int) -> b -> io a) ->
               (SubProcess -> io b) ->
               io a
  withPopen2 cmd error finalise f = do
    Right subp <- popen2 cmd | Left e => error e
    res <- f subp
    exitCode <- popen2Wait subp
    finalise exitCode res

  export
  externalXZ' : HasIO n => MonadError XZUsabilityError n => n XZ
  externalXZ' = assert_total $ do
    checkUsable Cat "abcd" (== "abcd")
    checkUsable Xz {arg="--version"} "abcd" $ (> 0) . length
    checkUsable Wc "abcd" $ (== "4") . trim
    pure CallExternXZ

    where
      covering
      checkUsable : (cmd : XZCmd) -> {default "" arg : String} -> (stdin : String) -> (stdoutCheck : String -> Bool) -> n ()
      checkUsable cmd stdin stdoutCheck = do
        let errHandler = \_ => throwError $ CmdIsNotUsable cmd
        let ecHandler : forall a. Int -> a -> n a := \case
          0  => pure
          ec => const $ throwError $ CmdExitsWrong cmd ec
        withPopen2 "\{cmd} \{arg}" errHandler ecHandler $ \subp => do
          Right () <- fPutStr subp.input stdin | _ => throwError $ CmdCan'tRead cmd
          closeFile subp.input
          Right stdout <- fRead subp.output    | _ => throwError $ Can'tReadFromCmd cmd
          closeFile subp.output
          let True = stdoutCheck stdout        | _ => throwError $ CmdWritesWrong cmd stdout
          pure ()

      [CallExternXZ] XZ where

        xzStrLen ss = do
          let act : IO $ Maybe Nat = do
            withPopen2 "\{Xz} | \{Wc}" (const $ pure $ the (Maybe Nat) Nothing) (\_ => pure) $ \subp => do
              Right _ <- for @{Compose} ss $ fPutStr subp.input | _ => pure Nothing
              closeFile subp.input
              Right n <- assert_total fRead subp.output         | _ => pure Nothing
              closeFile subp.output
              pure $ parsePositive n
          let _ = Monoid.Additive
          fromMaybe (foldMap length ss) $ unsafePerformIO act

        xzFileLen fs = do
          let fss = fs <&> \pth => (pth, escapeArg $ interpolate pth)
          for_ fss $ \(pth, fn) => do
            Right f <- openFile fn Read | Left e => throwError $ FileIOError pth e
            closeFile f
          let cmd = "\{Cat} \{joinBy " " $ snd <$> fss} | \{Xz} | \{Wc}"
          Right f <- popen cmd Read        | _ => throwError $ Couldn'tCompress fs
          Right n <- assert_total fRead f  | _ => throwError $ Couldn'tReadBytesCount
          0 <- pclose f                    | _ => throwError $ Couldn'tCompress fs
          let Just n = parsePositive n     | _ => throwError $ BadBytesCount n
          pure n

  export
  externalXZ : HasIO n => n $ Either XZUsabilityError XZ
  externalXZ = runEitherT {e=XZUsabilityError} {m=n} externalXZ'

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
StringNCD : XZ => NCD Prelude.id String
StringNCD = %search

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

export
FileNCD : XZ => HasIO m => MonadError XZFileError m => NCD m AnyFile
FileNCD = %search
