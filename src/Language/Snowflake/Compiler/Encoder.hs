module Language.Snowflake.Compiler.Encoder where

import Prelude hiding (Ordering(..))

import Language.Snowflake.Compiler.Types

import Control.Monad.RWS

import Data.Char
import Data.Word
import Data.List (genericLength)
import Data.Version
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put

import Data.Time.Clock.POSIX

import Crypto.Hash

type Encoder a = a -> Put

encodeMagic :: Encoder ()
encodeMagic () = mapM_ (putWord8 . fromIntegral) (map ord "snowflake")

encodeVersion :: Encoder Version
encodeVersion v = mapM_ (putWord8 . fromIntegral) (versionBranch v)

encodeTimestamp :: Encoder Word32
encodeTimestamp = putWord32be

encodeSegment :: Encoder Segment
encodeSegment (Segment c s i) = do
    encodeConstantTable c
    encodeSymbolTable s
    encodeInstructions i

encodeConstantTable :: Encoder [Constant]
encodeConstantTable c = do
    putWord32be (genericLength c)
    mapM_ encodeConstant c

encodeSymbolTable :: Encoder [Name]
encodeSymbolTable s = do
    putWord32be (genericLength s)
    mapM_ encodeSymbol s

encodeConstant :: Encoder Constant
encodeConstant NoneConst         = putWord8 0x00
encodeConstant (IntConst n)      = putWord8 0x01 >> putInt64be n
encodeConstant (FloatConst x)    = putWord8 0x02 >> putFloatbe x
encodeConstant (BoolConst False) = putWord8 0x03 >> putWord8 0x00
encodeConstant (BoolConst True)  = putWord8 0x03 >> putWord8 0x01
encodeConstant (StrConst s)      = putWord8 0x04 >> putWord32be (fromIntegral $ BS.length bs) >> putLazyByteString bs
    where bs = packUTF8 s
encodeConstant (FuncConst i)     = putWord8 0x05 >> putWord32be i

encodeSymbol :: Encoder Name
encodeSymbol sym = putWord32be (fromIntegral $ BS.length bsym) >> putLazyByteString bsym
    where bsym = packUTF8 sym

encodeInstructions :: Encoder [Instr]
encodeInstructions is = putWord32be (genericLength is) >> mapM_ encodeInstr is

encodeInstr :: Encoder Instr
encodeInstr NOP = putWord32be 0x00
encodeInstr POP = putWord32be 0x01
encodeInstr NOT = putWord32be 0x10
encodeInstr AND = putWord32be 0x11
encodeInstr OR  = putWord32be 0x12
encodeInstr ADD = putWord32be 0x20
encodeInstr SUB = putWord32be 0x21
encodeInstr MUL = putWord32be 0x22
encodeInstr DIV = putWord32be 0x23
encodeInstr POW = putWord32be 0x24
encodeInstr POS = putWord32be 0x25
encodeInstr NEG = putWord32be 0x26
encodeInstr LT  = putWord32be 0x30
encodeInstr LE  = putWord32be 0x31
encodeInstr EQ  = putWord32be 0x32
encodeInstr NEQ = putWord32be 0x33
encodeInstr GE  = putWord32be 0x34
encodeInstr GT  = putWord32be 0x35
encodeInstr RETURN = putWord32be 0x02
encodeInstr IF = putWord32be 0x03
encodeInstr (CALL n)        = putWord32be 0x04 >> putWord32be n
encodeInstr (BUILD_LIST n)  = putWord32be 0x05 >> putWord32be n
encodeInstr (BUILD_TUPLE n) = putWord32be 0x06 >> putWord32be n
encodeInstr (STORE n)       = putWord32be 0x07 >> putWord32be n
encodeInstr (LOAD n)        = putWord32be 0x08 >> putWord32be n
encodeInstr (LOAD_CONST n)  = putWord32be 0x09 >> putWord32be n
encodeInstr (JUMP n)        = putWord32be 0x0A >> putInt32be n
encodeInstr (ITER n)        = putWord32be 0x0B >> putInt32be n

encodeHeader :: Encoder (ByteString, Bytecode)
encodeHeader (hash, (Bytecode _ _ ts v)) = do
    encodeMagic ()
    encodeVersion v
    encodeTimestamp ts
    putLazyByteString hash

encodeBody :: Encoder Bytecode
encodeBody (Bytecode ss tl _ _) = do
    encodeSegment tl
    mapM_ encodeSegment ss

encodeBytecode :: Encoder Bytecode
encodeBytecode bc = do
    let bs = runPut (encodeBody bc)
    let hash = hashMD5 bs
    encodeHeader (hash, bc)
    putLazyByteString bs

encodeToFile :: FilePath -> Bytecode -> IO ()
encodeToFile path bytecode = do
    let encoded = runPut (encodeBytecode bytecode)
    BS.writeFile path encoded
