module Test.Common

import Data.Nat
import Data.Vect

import public Hedgehog

import public Information.Distance.NCD.XZ

import public Language.Reflection

import System

%default total

byteChar : Gen Char
byteChar = char $ constantFrom 'a' '\0' '\xFF'

bytesString' : {default 10000 maxlen : Nat} -> Gen String
bytesString' = string (linear 0 maxlen) byteChar

export
bytesString : {default 10000 maxlen : Nat} -> Gen String
bytesString = do
  n <- nat $ linear 0 $ divNatNZ maxlen 9 %search
  rep <- bytesString' {maxlen = divNatNZ maxlen (S n) %search}
  ss <- vect n $ bytesString' {maxlen=9}
  pure $ concat $ ss <&> \s => rep ++ s

cut : String -> Gen String
cut s = do
  let indGen = nat $ constant 0 $ length s
  (i, j) <- [| (indGen, indGen) |]
  let (i, j) = (min i j, max i j)
  let l = substr 0 i s
  let r = substr j (length s `minus` j) s
  pure $ l ++ r

perm : String -> Gen String
perm s = do
  let indGen = nat $ constant 0 $ length s
  (i, j) <- [| (indGen, indGen) |]
  let (i, j) = (min i j, max i j)
  let l = substr 0 i s
  let m = substr i (j `minus` i) s
  let r = substr j (length s `minus` j) s
  element
    [ l ++ r ++ m
    , m ++ l ++ r
    , m ++ r ++ l
    , r ++ l ++ m
    , r ++ m ++ l
    ]

mapCh : String -> Gen String
mapCh s = do
  f <- function byteChar
  pure $ pack $ apply f <$> unpack s

export
bytesString2 : Gen (String, String)
bytesString2 = frequency
  [ (the Nat 1,) [| (bytesString, bytesString) |]
  , (the Nat 10,) $ do
      (s, changes) <- [| (bytesString, list (linear 0 25) $ element [cut, perm, mapCh]) |]
      s' <- foldlM (flip apply) s changes
      element [(s, s'), (s', s)]
  ]

export
testProperty' : (name : String) -> (XZ => Property) -> IO ()
testProperty' name prop = do
  Right _ <- externalXZ | Left e => die "Error during preparation of using XZ: \{e}"
  test $ pure $ MkGroup "ncd" $ pure (fromString name, prop)

export
cleanupQ : TTImp -> TTImp
cleanupQ = mapTTImp $ \case
  IVar fc (NS _ n) => IVar fc n
  l@(ILam _ _ _ (Just $ UN _) _ _) => l
  ILam _ _ _ _ _ ty => ty
  INamedApp _ e _ (IHole _ _) => e
  e@(INamedApp _ _ (UN n) _) => e
  INamedApp _ e _ _ => e
  e => e

export %macro
testProperty : (XZ => Property) -> Elab $ IO ()
testProperty prop = do
  q <- quote {val = XZ => Property} prop
  pure $ testProperty' (show $ cleanupQ q) prop
