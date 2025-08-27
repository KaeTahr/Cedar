module Cedar.Semantic.C where

import Numeric.Natural

-- C Standard: ISO C99

-- use authority from compcert!!

-- compcert supports almost all of the ISO C 2011, ISO C 1999

-- https://compcert.org/doc/html/compcert.cfrontend.Ctypes.html

-- Inductive type : Type :=
--   | Tvoid: type (* the void type *)
--   | Tint: intsize -> signedness -> attr -> type (* integer types *)
--   | Tlong: signedness -> attr -> type (* 64-bit integer types *)
--   | Tfloat: floatsize -> attr -> type (* floating-point types *)
--   | Tpointer: type -> attr -> type (* pointer types (*ty) *)
--   | Tarray: type -> Z -> attr -> type (* array types (ty[len]) *)
--   | Tfunction: typelist -> type -> calling_convention -> type (* function types *)
--   | Tstruct: ident -> attr -> type (* struct types *)
--   | Tunion: ident -> attr -> type (* union types *)
-- with typelist : Type :=
--   | Tnil: typelist
--   | Tcons: type -> typelist -> typelist.

-- Inductive signedness : Type :=
--   | Signed: signedness
--   | Unsigned: signedness.

-- Inductive intsize : Type :=
--   | I8: intsize
--   | I16: intsize
--   | I32: intsize
--   | IBool: intsize.

-- Float types come in two sizes: 32 bits (single precision) and 64-bit (double precision).

-- Inductive floatsize : Type :=
--   | F32: floatsize
--   | F64: floatsize.

-- Every type carries a set of attributes. Currently, only two attributes are modeled: volatile and _Alignas(n) (from ISO C 2011).

-- Record attr : Type := mk_attr {
--   attr_volatile: bool;
--   attr_alignas: option N (* log2 of required alignment *)
-- }.

-- Definition noattr := {| attr_volatile := false; attr_alignas := None |}.

-- Record calling_convention : Type := mkcallconv {
--   cc_vararg: option Z; (* variable-arity function (+ number of fixed args) *)
--   cc_unproto: bool; (* old-style unprototyped function *)
--   cc_structret: bool (* function returning a struct *)
-- }.




data Signedness = Signed | Unsigned deriving (Show)
data IntSize = I8 | I16 | I32 | I64 | IBool deriving (Show) -- I64 in place of long. Can apply signedness to Bool but does nothing
data FloatSize = F32 | F64 | LongDouble deriving (Show) -- 64 represents the standard double and LongDouble size is platform dependent and strangely missing from CompCert

-- Define the Attr record
type N = Natural -- N for natural numbers


data Attr = Attr
  { attrVolatile :: Bool
  , attrAlignas :: Maybe N -- Represents an optional alignment (log2 of required alignment)
  } deriving (Show, Eq)

noattr :: Attr
noattr = Attr
  { attrVolatile = False
  , attrAlignas  = Nothing
  }

data CallingConvention = CallingConvention
  { ccVararg    :: Maybe Int  -- Variable-arity function (+ number of fixed args)
  , ccUnproto   :: Bool       -- Old-style unprototyped function
  , ccStructret :: Bool       -- Function returning a struct
  } deriving (Show, Eq)

type FieldName = String
type TypeVar = String

-- create variables/constants for intsize and pointer size
-- fix basicmemtypes

data CType = 
    Void 
    | Int IntSize Signedness Attr -- char is a type of int
    | Long Signedness Attr 
    | Float FloatSize Attr
    | Pointer CType Attr
    | Array CType Int Attr -- arrays should only have a positive length
    | Function [CType] CType CallingConvention
    | Struct [(FieldName, CType)]    -- differ to CompCert
    | Union [(FieldName, CType)]     -- differ to CompCert (no fields as C has untagged Unions)
    deriving (Show)

-- more:
    -- -- not in CompCert
    -- | Enum [FieldName]

    -- ident is just a positive integer (how does that provide enough information for Struct and Union?)