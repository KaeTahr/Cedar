-- protent from dargent
-- cogent/src/Cogent/Dargent/Core.hs

{-# LANGUAGE DeriveFunctor #-}
module Cedar.Layout.Core
  ( DataLayout'(..), DataLayout(..)
  , alignLayout', dataLayoutSizeBits'

  ) where

import qualified Data.Map as M
import Cedar.Compat.Basic (Size, wordSizeBits)
import Cedar.CodeGen.Allocation
  ( BitRange(..), AlignedBitRange(..)
  , rangeToAlignedRanges, alignSize
  )
import Cedar.Layout.Surface (Endianness)

-- Core datalayout (BitRange or AlignedBitRange via fmap)
data DataLayout' bits
  = PrimLayout    { bitsDL :: bits, endianness :: Endianness }
  | RecordLayout  (M.Map String (DataLayout' bits))
  deriving (Show, Eq, Functor)

data DataLayout bits
  = Layout (DataLayout' bits)
  deriving (Show, Eq, Functor)

-- size of a BitRange-backed layout (hi - lo)
dataLayoutSizeBits' :: DataLayout' BitRange -> Size
dataLayoutSizeBits' (PrimLayout (BitRange sz off) _) = sz              -- all prims are absolute here
dataLayoutSizeBits' (RecordLayout mp) =
  let hi = maximum (0 : [ off + sz | PrimLayout (BitRange sz off) _ <- M.elems mp ])
  in fromIntegral (alignSize 8 hi)  -- round up to byte

-- convert BitRange → [AlignedBitRange] (word lanes)
alignLayout' :: DataLayout' BitRange -> DataLayout' [AlignedBitRange]
alignLayout' = fmap (rangeToAlignedRanges wordSizeBits)
