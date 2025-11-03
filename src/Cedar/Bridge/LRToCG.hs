-- prepeares AST for codegen
-- integrates code from semantic module into codegen pipeline

module Cedar.Bridge.LRToCG
  ( layoutLRToCGStruct
  ) where

import           Data.List (intercalate)

import qualified Cedar.Semantic.LR as LR

import Cedar.CodeGen.Allocation (AlignedBitRange(..))
import Cedar.Layout.Surface (Endianness(..))
import Cedar.Layout.ToCodeGen (CGStruct(..), CGField(..))
import Cedar.Compat.Basic (Size(..))
import Data.Char (isAlphaNum, isDigit)


-- Convert LR.Layout to our CGStruct (fields with ABRs), flattening names with "__".
-- Arrays are expanded (uniform only) as field__0, field__1, ...
layoutLRToCGStruct :: String -> LR.Layout -> CGStruct
layoutLRToCGStruct typeName lr =
  let fields = flatten [] 0 lr
      maxLane =
        maximum (0 : [ toInt woff
                     | CGField _ rs _ _ <- fields
                     , AlignedBitRange _ _ woff <- rs
                     ])
      wordCount = maxLane + 1
  in CGStruct typeName wordCount fields

-- ===== flatten LR → leaf CGFields =====

flatten :: [String] -> Int -> LR.Layout -> [CGField]
flatten path baseOff lay = case lay of
  LR.Primitive (LR.Range off sizeBits) omega ->
    let absBit = baseOff + off
        abrs   = rangeToABRs absBit sizeBits
        name   = joinName path
        endi   = toEnd omega
    in  [ CGField name abrs endi False ]

  LR.Struct off fields _omega ->
    concat [ flatten (path ++ [fname]) (baseOff + off) sub
           | (fname, sub) <- fields
           ]

  LR.Array off elemTy len _omega ->
    let elemBits = LR.sizeOf elemTy
    in  concat [ flatten (path ++ [show i])
                       (baseOff + off + i * elemBits)
                       elemTy
               | i <- [0 .. len - 1] ]

  LR.Union off fields _omega ->
    concat [ flatten (path ++ [fname]) (baseOff + off) sub
           | (fname, sub) <- fields
           ]

-- ===== utilities =====

rangeToABRs :: Int -> Int -> [AlignedBitRange]
rangeToABRs start size = map toLane (byteSlices start size)
  where
    toLane (baseByte, bitOff, nBits) =
      let laneIndex = baseByte `div` 4
          intraByte = baseByte `mod` 4
          bitOff'   = bitOff + 8 * intraByte
      in  AlignedBitRange
            { bitSizeABR    = fromIntegral nBits
            , bitOffsetABR  = fromIntegral bitOff'
            , wordOffsetABR = fromIntegral laneIndex }

byteSlices :: Int -> Int -> [(Int, Int, Int)]
byteSlices o s
  | s <= 0    = []
  | otherwise =
      let nextByte = ((o `div` 8) + 1) * 8
          takeBits = if o + s <= nextByte then s else nextByte - o
      in  (o `div` 8, o `mod` 8, takeBits)
          : byteSlices (o + takeBits) (s - takeBits)

-- This can be done more elegantly
joinName :: [String] -> String
joinName = sanitize.intercalate "__"

sanitize :: String -> String
sanitize s =
  let s' = map (\c -> if isAlphaNum c then c else '_') s
  in case s' of
       []     -> "field"
       (c:cs) -> if isDigit c then '_' : c : cs else c : cs

toInt :: Size -> Int
toInt = fromIntegral

toEnd :: LR.Endianess -> Endianness
toEnd x = case x of
  LR.LE -> LE
  LR.BE -> BE
  LR.ME -> ME