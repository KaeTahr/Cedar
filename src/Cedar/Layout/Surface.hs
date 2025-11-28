-- Ported from dargent
-- cogent/src/Cogent/Dargent/Surface.hs

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
module Cedar.Layout.Surface
  ( Endianness(..)
  , DataLayoutSize(..), evalSize
  , DataLayoutExpr(..), DataLayoutExpr'(..)
  , pattern DLPrim, pattern DLRecord, pattern DLOffset
  , pattern DLEndian, pattern DLPtr
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)

-- little/big/machine
data Endianness = LE | BE | ME
  deriving (Show, Eq, Ord, Data, Typeable)

-- sizes in B/b with addition
data DataLayoutSize
  = Bytes Integer
  | Bits  Integer
  | Add   DataLayoutSize DataLayoutSize
  deriving (Show, Eq, Ord, Data, Typeable)

evalSize :: DataLayoutSize -> Integer
evalSize (Bytes b) = b * 8
evalSize (Bits  b) = b
evalSize (Add a b) = evalSize a + evalSize b

-- knot-tying surface layout
data DataLayoutExpr' e
  = Prim    DataLayoutSize
  | Record  [(String, e)]
  | Offset  e DataLayoutSize
  | Endian  e Endianness
  | Ptr
  deriving (Show, Eq, Ord, Data, Typeable)

newtype DataLayoutExpr = DL { unDL :: DataLayoutExpr' DataLayoutExpr }
  deriving (Show, Eq, Ord, Data, Typeable)

pattern DLPrim s     = DL (Prim s)
pattern DLRecord fs  = DL (Record fs)
pattern DLOffset e s = DL (Offset e s)
pattern DLEndian e n = DL (Endian e n)
pattern DLPtr        = DL Ptr
