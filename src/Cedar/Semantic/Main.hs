-- Original Author: George Anderson

-- Old driver for first prototype before parsing, lexing and codegen was implemented

module Cedar.Semantic.Main where

import qualified Cedar.Semantic.BasicType as BMT
import qualified Cedar.Semantic.L as L
import qualified Cedar.Semantic.LR as LR
import qualified Cedar.Semantic.C as C
import qualified Cedar.Semantic.CR as CR
import qualified Cedar.Semantic.C_to_CR as C_to_CR
import qualified Cedar.Semantic.L_to_LR as L_to_LR
import qualified Cedar.Semantic.CR_LR_matching as CR_LR

-- C
studentC :: C.CType
studentC = C.Struct [
    ("Student Number", C.Int C.I32 C.Signed C.noattr), 
    ("DOB", C.Int C.I32 C.Signed C.noattr), 
    ("Grades", C.Struct [
        ("Maths", C.Int C.I32 C.Signed C.noattr)
        ("Physics", C.Int C.I32 C.Signed C.noattr)
        ])
    ]

-- L
studentL :: L.Layout
studentL = L.Struct (L.AbsOffset (L.Bit 0))
    [
    ("Student Number", L.Primitive (L.Byte 4) (L.AbsOffset (L.Byte 0)) L.LE),
    ("DOB", L.Primitive (L.Byte 4) (L.RelOffset "Grades" (L.After (L.Byte 0))) L.LE),
    ("Grades", L.Struct (L.RelOffset "Student Number" (L.After (L.Byte 2))) [
        ("Maths", L.Primitive (L.Byte 4) (L.AbsOffset (L.Byte 0)) L.BE),
        ("Physics", L.Primitive (L.Byte 4) (L.RelOffset "Maths" (L.After (L.Byte 0))) L.BE)
        ] L.ME)
    ] L.LE

-- reduction from C to CR
studentCR :: CR.CRType
studentCR = C_to_CR.reduction studentC

-- wellformedness check on L
wfL :: L.Result
wfL = L.wf studentL

-- -- reduction from L to LR
studentLR :: LR.Layout
studentLR = L_to_LR.reduction studentL

-- -- wellformedness check on LR
wfLR :: LR.Result
wfLR = LR.wf studentLR

-- -- A matching check between CR and LR
studentMatch :: Bool
studentMatch = CR_LR.matching BMT.defaultConfig studentCR studentLR

-- run tests:
-- cd .\code\
-- ghci Cedar\Semantic\Main.hs
-- main

main :: IO ()
main = do
    print studentC
    print studentCR
    print studentL
    print wfL
    print studentLR
    print wfLR
    print studentMatch
