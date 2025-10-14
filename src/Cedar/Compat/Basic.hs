{-# LANGUAGE ScopedTypeVariables #-}
module Cedar.Compat.Basic
  ( -- core aliases
    Size, VarName
    -- target-size constants
  , byteSizeBits, wordSizeBits, pointerSizeBits
    -- generic offsetting
  , Offsettable(..)
  ) where

-- | Basic aliases Dargent code assumes exist in Cogent.
type Size    = Integer
type VarName = String

-- | Size constants (bits). Adjust if you target a different ABI.
byteSizeBits    :: Size
byteSizeBits    = 8

-- | “word” lane used by bit-splitting helpers (Dargent uses a register-sized lane).
-- We choose 32 here to mirror their C code that operates on unsigned int lanes.
wordSizeBits    :: Size
wordSizeBits    = 32

-- | Pointer size for the target.
pointerSizeBits :: Size
pointerSizeBits = 64

-- | Things we can offset by a bit count (used heavily during desugaring / allocation).
class Offsettable a where
  offset :: Size -> a -> a
