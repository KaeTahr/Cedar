{-# LANGUAGE DeriveGeneric #-}
module Cedar.CodeGen.Boundary
  ( -- core IR
    Prim(..), CType(..), Endian(..)
  , Range(..), CField(..), CBoundStruct(..)
    -- helpers
  , bytesNeeded
  , endBit
  , mkRange
  , totalSizeBitsFromFields
  ) where

import GHC.Generics (Generic)

-- ====== Boundary types (CR + LR merged) ======

data Prim = I8 | I16 | I32 | I64 | F32 | F64 | Ptr
  deriving (Eq, Show, Generic)

data CType
  = CPrim Prim
  | CPointer CType
  | CStruct [(String, CType)]
  deriving (Eq, Show, Generic)

data Endian = LE | BE deriving (Eq, Show, Generic)

-- Absolute bit range into the backing byte array
data Range = Range { offsetBits :: Int, sizeBits :: Int }
  deriving (Eq, Show, Generic)

data CField = CField
  { fName   :: String
  , fType   :: CType
  , fRange  :: Range           -- absolute bit range in the backing blob
  , fEndian :: Maybe Endian    -- for word/float fields; Nothing for raw bit blobs
  }
  deriving (Eq, Show, Generic)

data CBoundStruct = CBoundStruct
  { sName     :: String
  , sSizeBits :: Int           -- total bits needed (we’ll round to bytes for the array)
  , sFields   :: [CField]
  }
  deriving (Eq, Show, Generic)

-- ====== Helpers ======

bytesNeeded :: Int -> Int
bytesNeeded bits = (bits + 7) `div` 8

endBit :: Range -> Int
endBit (Range o s) = o + s

mkRange :: Int -> Int -> Range
mkRange = Range

-- Compute a conservative total size from fields: ceil(maxEndBit / 8)*8
totalSizeBitsFromFields :: [CField] -> Int
totalSizeBitsFromFields fs =
  let maxEnd = maximum (0 : map (endBit . fRange) fs)
      roundUp8 b = ((b + 7) `div` 8) * 8
  in roundUp8 maxEnd