{-# LANGUAGE LambdaCase #-}

module Language.Snowflake.Compiler.Decoder where

import Prelude hiding (Ordering(..))

import Language.Snowflake.Compiler.Types

import Control.Monad.RWS
import Control.Applicative (many)

import Data.Char
import Data.Word
import Data.List (genericLength)
import Data.Version

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get hiding (Decoder)

import Data.Time.Clock.POSIX

import Paths_snowflake (version)

type Decoder a = Get a

checkMagic :: Decoder ()
checkMagic = do
    magic <- replicateM (length "snowflake") (fromIntegral <$> getWord8)
    if magic == map ord "snowflake" then
        return ()
    else
        fail "Magic string \"snowflake\" undetected"

decodeVersion :: Decoder Version
decodeVersion = do
    v <- replicateM 4 (fromIntegral <$> getWord8)
    if v == versionBranch version then
        return version
    else
        fail "Wrong version branch"

decodeTimestamp :: Decoder Word32
decodeTimestamp = getWord32be

decodeSegment :: Decoder Segment
decodeSegment = Segment <$> decodeConstantTable <*> decodeSymbolTable <*> decodeInstructions

decodeConstantTable :: Decoder [Constant]
decodeConstantTable = do
    n <- fromIntegral <$> getWord32be
    replicateM n decodeConstant

decodeSymbolTable :: Decoder [Name]
decodeSymbolTable = do
    n <- fromIntegral <$> getWord32be
    replicateM n decodeSymbol

decodeConstant :: Decoder Constant
decodeConstant = getWord8 >>= \case
    0x00 -> return NoneConst
    0x01 -> IntConst <$> getInt64be
    0x02 -> FloatConst <$> getFloatbe
    0x03 -> getWord8 >>= \case
        0x00 -> return (BoolConst False)
        0x01 -> return (BoolConst True)
    0x04 -> do
        size <- fromIntegral <$> getWord32be
        val <- unpackUTF8 <$> getLazyByteString size
        return (StrConst val)
    0x05 -> FuncConst <$> getWord32be
    b -> fail $ "Unknown constant " ++ show b

decodeSymbol :: Decoder Name
decodeSymbol = do
    size <- fromIntegral <$> getWord32be
    unpackUTF8 <$> getLazyByteString size

decodeInstructions :: Decoder [Instr]
decodeInstructions = do
    opcount <- fromIntegral <$> getWord32be
    replicateM opcount decodeInstr

decodeInstr :: Decoder Instr
decodeInstr = getWord32be >>= \case
    0x00 -> return NOP
    0x01 -> return POP
    0x10 -> return NOT
    0x11 -> return AND
    0x12 -> return OR
    0x20 -> return ADD
    0x21 -> return SUB
    0x22 -> return MUL
    0x23 -> return DIV
    0x24 -> return POW
    0x25 -> return POS
    0x26 -> return NEG
    0x30 -> return LT
    0x31 -> return LE
    0x32 -> return EQ
    0x33 -> return NEQ
    0x34 -> return GE
    0x35 -> return GT
    0x02 -> return RETURN
    0x03 -> return IF
    0x04 -> CALL <$> getWord32be
    0x05 -> BUILD_LIST <$> getWord32be
    0x06 -> BUILD_TUPLE <$> getWord32be
    0x07 -> STORE <$> getWord32be
    0x08 -> LOAD <$> getWord32be
    0x09 -> LOAD_CONST <$> getWord32be
    0x0A -> JUMP <$> getInt32be
    0x0B -> ITER <$> getInt32be
    b -> fail $ "Unable to decode instruction " ++ show b

decodeHeader :: Decoder (Version, Word32, ByteString)
decodeHeader = do
    checkMagic
    v <- decodeVersion
    t <- decodeTimestamp
    h <- getLazyByteString 32
    return (v, t, h)

decodeBody :: Decoder (Segment, [Segment])
decodeBody = do
    tl <- decodeSegment
    ss <- many decodeSegment
    return (tl, ss)

decodeBytecode :: Decoder Bytecode
decodeBytecode = do
    (v, t, h) <- decodeHeader
    remaining <- lookAhead getRemainingLazyByteString
    if hashMD5 remaining == h then do
        (tl, ss) <- decodeBody
        return (Bytecode ss tl t v)
    else
        fail "File corrupted: failed to match MD5 hash"

decodeFromFile :: FilePath -> IO Bytecode
decodeFromFile path = do
    bs <- BS.readFile path
    case runGetOrFail decodeBytecode bs of
        Right (_, _, bc)  -> return bc
        Left  (_, _, err) -> fail err
