module Cedar.Frontend.Subset.Lower
  ( lowerToCL ) where

import Cedar.Frontend.Subset.AST
import qualified Cedar.Semantic.C as C
import qualified Cedar.Semantic.L as L

lowerToCL :: TopDecl -> (C.CType, L.Layout)
lowerToCL (TypeDecl   _ ty) = (toC ty, toLAbs 0 ty)
lowerToCL (LayoutDecl _ ty) = (toC ty, toLAbs 0 ty)

toC :: PType -> C.CType
toC (TPrim p)          = C.Int (toCWidth p) (toCSigned p) C.noattr
toC (TArray t n)       = C.Array (toC t) n C.noattr
toC (TRecord _ fs)     = C.Struct [ (pfName f, toC (pfType f)) | f <- fs ]

mkOff :: POffset -> L.Offset
mkOff (AbsB n)            = L.AbsOffset (L.Byte n)
mkOff (RelAfter  s n)     = L.RelOffset s (L.After  (L.Byte n))
mkOff (RelBefore s n)     = L.RelOffset s (L.Before (L.Byte n))

toCWidth :: Prim -> C.IntSize
toCWidth U8  = C.I8;  toCWidth U16 = C.I16; toCWidth U32 = C.I32; toCWidth U64 = C.I64
toCWidth I8  = C.I8;  toCWidth I16 = C.I16; toCWidth I32 = C.I32; toCWidth I64 = C.I64

toCSigned :: Prim -> C.Signedness
toCSigned U8  = C.Unsigned; toCSigned U16 = C.Unsigned; toCSigned U32 = C.Unsigned; toCSigned U64 = C.Unsigned
toCSigned I8  = C.Signed;   toCSigned I16 = C.Signed;   toCSigned I32 = C.Signed;   toCSigned I64 = C.Signed

toLAbs :: Int -> PType -> L.Layout
toLAbs base (TPrim p)      =
  L.Primitive (L.Byte (primBytes p)) (L.AbsOffset (L.Byte base)) L.LE
toLAbs base (TArray t n)   =
  L.Array (L.AbsOffset (L.Byte base)) (toLAbs 0 t) n L.LE
toLAbs _    (TRecord off fs) =
  L.Struct (mkOff off) [ (pfName f, fieldL f) | f <- fs ] L.LE

fieldL :: PField -> L.Layout
fieldL (PField _ ty off mend) =
  let o = mkOff off
      e = maybe L.LE (\x -> case x of LE -> L.LE; BE -> L.BE; ME -> L.ME) mend
  in case ty of
       TPrim p            -> L.Primitive (L.Byte (primBytes p)) o e
       TArray t n         -> L.Array o (toLAbs 0 t) n e
       TRecord off' fs'   -> L.Struct (mkOff off') [ (pfName f, fieldL f) | f <- fs' ] e

primBytes :: Prim -> Int
primBytes U8 = 1; primBytes U16 = 2; primBytes U32 = 4; primBytes U64 = 8
primBytes I8 = 1; primBytes I16 = 2; primBytes I32 = 4; primBytes I64 = 8

toLE :: Maybe Endian -> L.Endianess
toLE Nothing  = L.LE
toLE (Just LE)= L.LE
toLE (Just BE)= L.BE
toLE (Just ME)= L.ME
