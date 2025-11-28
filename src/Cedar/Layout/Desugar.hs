-- ported from dargent
-- cogent/src/Cogent/Dargent/Desugar.hs

module Cedar.Layout.Desugar
  ( defaultRecordLayout
  , recordLayoutFromRanges
  ) where

import qualified Data.Map as M
import Cedar.Compat.Basic (Size)
import Cedar.CodeGen.Allocation (BitRange(..), alignSize)
import Cedar.Layout.Surface (Endianness(..))
import Cedar.Layout.Core

-- Sequential byte-aligned default: field i at offset (i * sizeBytes) * 8
-- Caller supplies (fieldName, sizeBits) for each field’s primitive size.
defaultRecordLayout
  :: [(String, Size)]           -- field name, size in bits
  -> DataLayout' BitRange
defaultRecordLayout fields =
  let place (accOff, accMap) (nm, sz) =
        let off   = alignSize 8 accOff
            entry = (nm, PrimLayout (BitRange sz off) ME)
        in (off + sz, M.insert nm entry accMap)
      (_, mp) = foldl place (0, M.empty) fields
  in RecordLayout mp

-- If bit ranger are already computer exactly per field:
recordLayoutFromRanges
  :: [(String, BitRange, Endianness)]
  -> DataLayout' BitRange
recordLayoutFromRanges triples =
  RecordLayout (M.fromList [ (n, PrimLayout br end) | (n, br, end) <- triples ])
