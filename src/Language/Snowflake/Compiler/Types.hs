{-# LANGUAGE TemplateHaskell #-}

module Language.Snowflake.Compiler.Types
  ( Instr(..)
  , Bytecode(..), bcSegments, bcTopLevel, bcTimestamp, bcVersion
  , Segment(..), segConstants, segSymbols, segInstrs
  , Constant(..)
  , Name
  , Message
  , showBytecode
  , hashMD5, packUTF8, unpackUTF8
  , prettyPrintCode
  ) where

import Language.Snowflake.Parser.AST

import Control.Lens
import Control.Monad (forM_)

import Data.Word
import Data.Int
import Data.Char
import Data.List
import Data.Version (Version, showVersion)

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8

import Crypto.Hash

type Message = String

data Instr
    = NOP
    | POP
    | NOT | AND | OR
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG
    | LT | LE | EQ | NEQ | GE | GT
    | RETURN
    | IF
    | CALL       Word32
    | BUILD_LIST Word32
    | STORE      Word32
    | LOAD       Word32
    | LOAD_CONST Word32
    | JUMP       Int32
    | ITER       Int32
    deriving (Eq, Show)

data Constant
    = IntConst Int64
    | FloatConst Float
    | BoolConst Bool
    | StrConst String
    | FuncConst Word32
    | NoneConst
    deriving Eq

instance Show Constant where
    show (IntConst n) = show n
    show (FloatConst x) = show x
    show (BoolConst b) = show b
    show (StrConst s) = show s
    show (FuncConst segIndex) = "<<function " ++ show segIndex ++ ">>"
    show NoneConst = "None"

data Segment = Segment
    { _segConstants :: [Constant]
    , _segSymbols   :: [Name]
    , _segInstrs    :: [Instr] }
    deriving (Eq, Show)
makeLenses ''Segment

data Bytecode = Bytecode
    { _bcSegments  :: [Segment]
    , _bcTopLevel  :: Segment
    , _bcTimestamp :: Word32
    , _bcVersion   :: Version }
    deriving (Eq, Show)
makeLenses ''Bytecode

showBytecode :: Bytecode -> String
showBytecode (Bytecode ss (Segment c s i) t v) = intercalate "\n" $
        [ "top level: "
        , "\tconstants: " ++ show c
        , "\tsymbols:   " ++ show s
        ] ++ map (('\t':) . showInstr) (zip [0..] i) ++
        [ "segments: "
        ] ++ map (('\t':) . showSeg) (zip [0..] ss) ++
        [ "version:   " ++ showVersion v
        , "timestamp: " ++ show t
        ]
    where showInstr (i, instr) = '\t' : show i ++ (replicate (4 - length (show i)) ' ') ++ show instr
          showSeg (i, (Segment constants symbols instrs)) =
              intercalate "\n" $ ["segment " ++ show i] ++ map ("\t\t" ++)
                  [ "constants: " ++ show constants
                  , "symbols:   " ++ show symbols
                  ] ++ map (("\t\t" ++) . showInstr) (zip [0..] instrs)

hashMD5 :: ByteString -> ByteString
hashMD5 = packUTF8 . show . go . C8.pack . map (chr . fromIntegral) . BS.unpack
    where go = hash :: C8.ByteString -> Digest MD5

packUTF8 :: String -> ByteString
packUTF8 = BS.pack . UTF8.encode

unpackUTF8 :: ByteString -> String
unpackUTF8 = UTF8.decode . BS.unpack

prettyPrintCode :: Segment -> IO ()
prettyPrintCode (Segment constants symbols instrs) =
    forM_ (zip [0..] instrs) $ \ (idx, instr) -> do
        putStr (show idx ++ " ")
        putStr $ replicate (4 - length (show idx)) ' ' ++ (show instr)
        case instr of
            STORE      addr -> putStr (" (" ++ genericIndex symbols addr ++ ")")
            LOAD       addr -> putStr (" (" ++ genericIndex symbols addr ++ ")")
            LOAD_CONST addr -> putStr (" (" ++ show (genericIndex constants addr) ++ ")")
            _               -> return ()
        putStrLn ""
