module Test.Common

import public Hedgehog

import public Information.Distance.NCD.XZ

import public Language.Reflection

import System

%default total

export
bytesString : Gen String
bytesString = string (linear 0 10000) $ char $ constantFrom 'a' '\0' '\xFF'

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
