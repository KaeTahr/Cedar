module Cedar.Frontend.Subset.AST where

data Prim
  = U8 | U16 | U32 | U64
  | I8 | I16 | I32 | I64
  deriving (Eq,Show)

data Endian = LE | BE | ME
  deriving (Eq,Show)

data POffset
  = AbsB Int
  | RelAfter  String Int
  | RelBefore String Int
  deriving (Eq,Show)

data PType
  = TPrim Prim
  | TArray PType Int
  | TRecord POffset [PField]
  deriving (Eq,Show)

data PField = PField
  { pfName   :: String
  , pfType   :: PType
  , pfOff    :: POffset
  , pfEndian :: Maybe Endian
  } deriving (Eq,Show)

data TopDecl
  = TypeDecl   String PType
  | LayoutDecl String PType
  deriving (Eq,Show)
