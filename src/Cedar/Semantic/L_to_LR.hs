-- Original Author: Goerge Anderson

module Cedar.Semantic.L_to_LR where

import qualified Cedar.Semantic.L as L
import qualified Cedar.Semantic.LR as LR

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Except

-- | The main reduction function: converts L.Layout to LR.Layout
reduction :: L.Layout -> LR.Layout
reduction layout = case runExcept (reductionLayout layout []) of
    Right lrLayout -> lrLayout
    Left errMsg -> error errMsg

-- | Reduces L.Layout to LR.Layout
reductionLayout :: L.Layout -> [Map L.FieldName L.Layout] -> Except String LR.Layout
reductionLayout layout fieldStack = case layout of
    L.Primitive size offset endianess -> do
        lrOffset <- resolveOffset offset fieldStack
        let lrSize = L.extractBits size
            lrEndianess = convertEndianess endianess
        return $ LR.Primitive (LR.Range lrOffset lrSize) lrEndianess

    L.Array offset lLayout len endianess -> do
        lrOffset <- resolveOffset offset fieldStack
        lrLayout <- reductionLayout lLayout fieldStack
        let lrEndianess = convertEndianess endianess
        return $ LR.Array lrOffset lrLayout len lrEndianess

    L.Struct offset fields endianess -> do
        lrOffset <- resolveOffset offset fieldStack
        -- Create a map of field names to L.Layouts for the current composite
        let fieldLayouts = Map.fromList fields
        -- Push the current fieldLayouts onto the fieldStack
        let newFieldStack = fieldLayouts : fieldStack
        -- Process fields using the newFieldStack
        lrFields <- mapM (\(fname, lLayout) -> do
            lrLayout <- reductionLayout lLayout newFieldStack
            return (fname, lrLayout)
            ) fields
        let lrEndianess = convertEndianess endianess
        return $ LR.Struct lrOffset lrFields lrEndianess

    L.Union offset fields endianess -> do
        lrOffset <- resolveOffset offset fieldStack
        let fieldLayouts = Map.fromList fields
        let newFieldStack = fieldLayouts : fieldStack
        lrFields <- mapM (\(fname, lLayout) -> do
            lrLayout <- reductionLayout lLayout newFieldStack
            return (fname, lrLayout)
            ) fields
        let lrEndianess = convertEndianess endianess
        return $ LR.Union lrOffset lrFields lrEndianess

-- | Resolves an L.Offset to an absolute offset in bits
resolveOffset :: L.Offset -> [Map L.FieldName L.Layout] -> Except String LR.Offset
resolveOffset offset fieldStack = case offset of
    L.AbsOffset ms -> return $ L.extractBits ms
    L.RelOffset fname order -> do
        -- Look up fname in fieldStack
        mRefLayout <- lookupInFieldStack fname fieldStack
        case mRefLayout of
            Just refLayout -> do
                refLRLayout <- reductionLayout refLayout fieldStack
                refOffset <- getLayoutOffset refLRLayout
                refSize <- getLayoutSize refLRLayout
                let adjustment = extractBitsFromOrder order
                case order of
                    L.After _  -> return $ refOffset + refSize + adjustment
                    L.Before _ -> return $ refOffset - refSize - adjustment
            Nothing -> throwError $ "Field not found in composite(s): " ++ fname

-- | Looks up a field name in the field stack
lookupInFieldStack :: L.FieldName -> [Map L.FieldName L.Layout] -> Except String (Maybe L.Layout)
lookupInFieldStack fname [] = return Nothing
lookupInFieldStack fname (flds:rest) =
    case Map.lookup fname flds of
        Just lLayout -> return $ Just lLayout
        Nothing -> lookupInFieldStack fname rest

-- | Retrieves the offset from an LR.Layout
getLayoutOffset :: LR.Layout -> Except String LR.Offset
getLayoutOffset layout = case layout of
    LR.Primitive (LR.Range offset _) _ -> return offset
    LR.Array offset _ _ _              -> return offset
    LR.Struct offset _ _               -> return offset
    LR.Union offset _ _                -> return offset

-- | Calculates the size in bits of an LR.Layout
getLayoutSize :: LR.Layout -> Except String LR.Size
getLayoutSize layout = case layout of
    LR.Primitive (LR.Range _ size) _ -> return size
    LR.Array _ lLayout len _         -> do
        elemSize <- getLayoutSize lLayout
        return $ elemSize * len
    LR.Struct _ fields _             ->
        if null fields
        then return 0
        else maximum <$> mapM (getLayoutEnd . snd) fields
    LR.Union _ fields _              ->
        if null fields
        then return 0
        else maximum <$> mapM getLayoutSize (map snd fields)

-- | Computes the end offset (offset + size) of an LR.Layout
getLayoutEnd :: LR.Layout -> Except String LR.Offset
getLayoutEnd layout = do
    offset <- getLayoutOffset layout
    size <- getLayoutSize layout
    return $ offset + size

-- | Extracts the bit adjustment from an L.Order
extractBitsFromOrder :: L.Order -> Int
extractBitsFromOrder order = case order of
    L.After ms  -> L.extractBits ms
    L.Before ms -> L.extractBits ms

-- | Converts L.Endianess to LR.Endianess
convertEndianess :: L.Endianess -> LR.Endianess
convertEndianess endianess = case endianess of
    L.BE -> LR.BE
    L.LE -> LR.LE
    L.ME -> LR.ME


-- Test functions

-- Test 1: Primitive with absolute offset
testPrimitiveAbsOffset :: IO ()
testPrimitiveAbsOffset = do
    let lLayout = L.Primitive (L.Byte 2) (L.AbsOffset (L.Byte 1)) L.BE
        actualLRLayout = reduction lLayout
    putStrLn "Test Primitive with Absolute Offset:"
    putStrLn $ "lLayout: " ++ show lLayout
    putStrLn $ "actualLRLayout: " ++ show actualLRLayout
    putStrLn ""

-- Test 2: Primitive with relative offset
testPrimitiveRelOffset :: IO ()
testPrimitiveRelOffset = do
    let lLayout1 = L.Primitive (L.Byte 1) (L.AbsOffset (L.Bit 0)) L.BE
        lLayout2 = L.Primitive (L.Byte 2) (L.RelOffset "field1" (L.After (L.Bit 0))) L.BE
        lStruct = L.Struct (L.AbsOffset (L.Bit 0)) [("field1", lLayout1), ("field2", lLayout2)] L.BE
        actualLRLayout = reduction lStruct
    putStrLn "Test Primitive with Relative Offset:"
    putStrLn $ "lLayout: " ++ show lStruct
    putStrLn $ "actualLRLayout: " ++ show actualLRLayout
    putStrLn ""

-- Test 3: Array
testArray :: IO ()
testArray = do
    let elementLayout = L.Primitive (L.Bit 4) (L.AbsOffset (L.Bit 0)) L.LE
        lLayout = L.Array (L.AbsOffset (L.Bit 0)) elementLayout 4 L.LE
        actualLRLayout = reduction lLayout
    putStrLn "Test Array:"
    putStrLn $ "lLayout: " ++ show lLayout
    putStrLn $ "actualLRLayout: " ++ show actualLRLayout
    putStrLn ""

-- Test 4: Struct with fields
testStruct :: IO ()
testStruct = do
    let lField1 = L.Primitive (L.Bit 8) (L.AbsOffset (L.Bit 0)) L.ME
        lField2 = L.Primitive (L.Bit 16) (L.RelOffset "field1" (L.After (L.Bit 0))) L.ME
        lLayout = L.Struct (L.AbsOffset (L.Bit 0)) [("field1", lField1), ("field2", lField2)] L.ME
        actualLRLayout = reduction lLayout
    putStrLn "Test Struct with Fields:"
    putStrLn $ "lLayout: " ++ show lLayout
    putStrLn $ "actualLRLayout: " ++ show actualLRLayout
    putStrLn ""

-- Test 5: Union with fields
testUnion :: IO ()
testUnion = do
    let lField1 = L.Primitive (L.Byte 1) (L.AbsOffset (L.Bit 0)) L.BE
        lField2 = L.Primitive (L.Byte 2) (L.AbsOffset (L.Bit 0)) L.BE
        lLayout = L.Union (L.AbsOffset (L.Bit 0)) [("field1", lField1), ("field2", lField2)] L.BE
        actualLRLayout = reduction lLayout
    putStrLn "Test Union with Fields:"
    putStrLn $ "lLayout: " ++ show lLayout
    putStrLn $ "actualLRLayout: " ++ show actualLRLayout
    putStrLn ""

-- Run all tests
runTests :: IO ()
runTests = do
    putStrLn "Running All Tests...\n"
    testPrimitiveAbsOffset
    testPrimitiveRelOffset
    testArray
    testStruct
    testUnion
    putStrLn "All Tests Completed."

-- cd .\code\
-- ghci Cedar\Semantic\L_to_LR.hs
-- main

-- Main function to execute tests
main :: IO ()
main = runTests