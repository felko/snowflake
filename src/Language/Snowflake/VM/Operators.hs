module Language.Snowflake.VM.Operators where

import Language.Snowflake.VM.Types

-- Numerical operators
addOp, subOp, mulOp, divOp, powOp :: Value -> Value -> VM Value
negOp :: Value -> VM Value

-- Boolean operators
andOp, orOp :: Value -> Value -> VM Value
notOp :: Value -> VM Value

-- Comparison operators
ltOp, leOp, eqOp, neqOp, geOp, gtOp :: Value -> Value -> VM Value

addOp (IntVal a) (IntVal b) = return $ IntVal (a + b)
addOp (IntVal a) (FloatVal b) = return $ FloatVal (realToFrac a + b)
addOp (FloatVal a) (IntVal b) = return $ FloatVal (a + realToFrac b)
addOp (FloatVal a) (FloatVal b) = return $ FloatVal (a + b)
addOp (StrVal a) (StrVal b) = return $ StrVal (a ++ b)
addOp _ _ = raise TypeError "ADD: wrong type"

subOp (IntVal a) (IntVal b) = return $ IntVal (a - b)
subOp (IntVal a) (FloatVal b) = return $ FloatVal (realToFrac a - b)
subOp (FloatVal a) (IntVal b) = return $ FloatVal (a - realToFrac b)
subOp (FloatVal a) (FloatVal b) = return $ FloatVal (a - b)
subOp _ _ = raise TypeError "SUB: wrong type"

mulOp (IntVal a) (IntVal b) = return $ IntVal (a * b)
mulOp (IntVal a) (FloatVal b) = return $ FloatVal (realToFrac a * b)
mulOp (FloatVal a) (IntVal b) = return $ FloatVal (a * realToFrac b)
mulOp (FloatVal a) (FloatVal b) = return $ FloatVal (a * b)
mulOp (IntVal n) (StrVal s) = return $ StrVal (mconcat $ replicate (fromIntegral n) s)
mulOp (StrVal s) (IntVal n) = return $ StrVal (mconcat $ replicate (fromIntegral n) s)
mulOp _ _ = raise TypeError "MUL: wrong type"

divOp (IntVal _) (IntVal 0) = raise ZeroDivisionError "DIV: divide by 0"
divOp (IntVal _) (FloatVal 0) = raise ZeroDivisionError "DIV: divide by 0"
divOp (FloatVal _) (IntVal 0) = raise ZeroDivisionError "DIV: divide by 0"
divOp (FloatVal _) (FloatVal 0) = raise ZeroDivisionError "DIV: divide by 0"
divOp (IntVal a) (IntVal b) = return $ FloatVal (realToFrac a / realToFrac b)
divOp (IntVal a) (FloatVal b) = return $ FloatVal (realToFrac a / b)
divOp (FloatVal a) (IntVal b) = return $ FloatVal (a / realToFrac b)
divOp (FloatVal a) (FloatVal b) = return $ FloatVal (a / b)
divOp _ _ = raise TypeError "DIV: wrong type"

powOp (IntVal a) (IntVal b) = return $ IntVal (a ^ b)
powOp (IntVal a) (FloatVal b) = return $ FloatVal (realToFrac a ** b)
powOp (FloatVal a) (IntVal b) = return $ FloatVal (a ^ b)
powOp (FloatVal a) (FloatVal b) = return $ FloatVal (a ** b)
powOp _ _ = raise TypeError "POW: wrong type"

negOp (IntVal x) = return $ IntVal (negate x)
negOp (FloatVal x) = return $ FloatVal (negate x)

andOp (BoolVal a) (BoolVal b) = return $ BoolVal (a && b)
andOp _ _ = raise TypeError "AND: wrong type"

orOp (BoolVal a) (BoolVal b) = return $ BoolVal (a || b)
orOp _ _ = raise TypeError "OR: wrong type"

notOp (BoolVal b) = return $ BoolVal (not b)

ltOp (IntVal a) (IntVal b) = return $ BoolVal (a < b)
ltOp (IntVal a) (FloatVal b) = return $ BoolVal (realToFrac a < b)
ltOp (FloatVal a) (IntVal b) = return $ BoolVal (a < realToFrac b)
ltOp (FloatVal a) (FloatVal b) = return $ BoolVal (a < b)
ltOp _ _ = raise TypeError "LT: wrong type"

leOp (IntVal a) (IntVal b) = return $ BoolVal (a <= b)
leOp (IntVal a) (FloatVal b) = return $ BoolVal (realToFrac a <= b)
leOp (FloatVal a) (IntVal b) = return $ BoolVal (a <= realToFrac b)
leOp (FloatVal a) (FloatVal b) = return $ BoolVal (a <= b)
leOp _ _ = raise TypeError "LE: wrong type"

eqOp (IntVal a) (IntVal b) = return $ BoolVal (a == b)
eqOp (IntVal a) (FloatVal b) = return $ BoolVal (realToFrac a == b)
eqOp (FloatVal a) (IntVal b) = return $ BoolVal (a == realToFrac b)
eqOp (FloatVal a) (FloatVal b) = return $ BoolVal (a == b)
eqOp _ _ = raise TypeError "EQ: wrong type"

neqOp (IntVal a) (IntVal b) = return $ BoolVal (a /= b)
neqOp (IntVal a) (FloatVal b) = return $ BoolVal (realToFrac a /= b)
neqOp (FloatVal a) (IntVal b) = return $ BoolVal (a /= realToFrac b)
neqOp (FloatVal a) (FloatVal b) = return $ BoolVal (a /= b)
neqOp _ _ = raise TypeError "NEQ: wrong type"

geOp (IntVal a) (IntVal b) = return $ BoolVal (a >= b)
geOp (IntVal a) (FloatVal b) = return $ BoolVal (realToFrac a >= b)
geOp (FloatVal a) (IntVal b) = return $ BoolVal (a >= realToFrac b)
geOp (FloatVal a) (FloatVal b) = return $ BoolVal (a >= b)
geOp _ _ = raise TypeError "GE: wrong type"

gtOp (IntVal a) (IntVal b) = return $ BoolVal (a > b)
gtOp (IntVal a) (FloatVal b) = return $ BoolVal (realToFrac a > b)
gtOp (FloatVal a) (IntVal b) = return $ BoolVal (a > realToFrac b)
gtOp (FloatVal a) (FloatVal b) = return $ BoolVal (a > b)
gtOp _ _ = raise TypeError "GT: wrong type"
