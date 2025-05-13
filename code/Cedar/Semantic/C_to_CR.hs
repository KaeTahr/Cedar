module Cedar.Semantic.C_to_CR where

import qualified Cedar.Semantic.C as C
import qualified Cedar.Semantic.CR as CR
import qualified Cedar.Semantic.BasicType as BMT

import qualified Data.Map as Map

-- reduction from C to CR (currently does not handle type variables - assume handled in preprocessor)
reduction :: C.CType -> CR.CRType

-- Void
reduction C.Void = CR.BasicMemType BMT.I8

-- Integer
reduction (C.Int intSize _ _) = case intSize of
    C.IBool            -> CR.BasicMemType BMT.IBool
    C.I8               -> CR.BasicMemType BMT.I8
    C.I16              -> CR.BasicMemType BMT.I16
    C.I32              -> CR.BasicMemType BMT.I32
    C.I64              -> CR.BasicMemType BMT.I64

-- Float and Double
reduction (C.Float floatSize _) = case floatSize of
    C.F32            -> CR.BasicMemType BMT.F32 
    C.F64            -> CR.BasicMemType BMT.F64
    C.LongDouble     -> CR.BasicMemType BMT.LongDouble

-- Pointer
reduction (C.Pointer _ _) = CR.BasicMemType BMT.Pointer

-- Function
reduction (C.Function _ _ _) = CR.BasicMemType BMT.Pointer

-- Array
reduction (C.Array cType len _) = CR.Array (reduction cType) len

-- Struct
reduction (C.Struct cfields) = CR.Struct crfields
  where
    crfields = map (\(fname, ctype) -> (fname, reduction ctype)) cfields

-- Union
reduction (C.Union cfields) = CR.Union crfields
  where
    crfields = map (\(fname, ctype) -> (fname, reduction ctype)) cfields