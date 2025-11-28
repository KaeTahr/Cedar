-- Original author: George Anderson

module Cedar.Semantic.BasicType where

-- bit sizes completely removes ambiguity
data BasicMemType = Pointer | IBool | I8 | I16 | I32 | I64 | F32 
    | F64 | LongDouble
    deriving (Show)

-- for adjusting 
data Config = Config
    { ldbSize :: Int
    , ptrSize :: Int
    }
    deriving (Show)

-- Define a default configuration
defaultConfig :: Config
defaultConfig = Config
    { ldbSize = 16 -- default to 16 bytes (128 bits)  for LongDouble
    , ptrSize = 8   -- default to 8 bytes (64 bits)  for Pointer
    }

-- Function to load configuration (could be extended to parse from file or CLI)
loadConfig :: Maybe Config -> Config
loadConfig = maybe defaultConfig id

-- Function to map BasicMemType to its size in bits with input in Bytes
size :: Config -> BasicMemType -> Int
size _ IBool              = 1 * 8 -- perhaps assign this 1 bit
size _ I8                 = 1 * 8 
size _ I16                = 2 * 8
size _ I32                = 4 * 8
size _ I64                = 8 * 8 
size _ F32                = 4 * 8
size _ F64                = 8 * 8
size config LongDouble  = ldbSize config * 8 -- typically 10 or 16 bytes
size config Pointer     = ptrSize config * 8