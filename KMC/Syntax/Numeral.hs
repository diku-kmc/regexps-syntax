module KMC.Syntax.Numeral
    ( IntegerBase(..)
    , numeralP
    ) where

import           Control.Monad                 (liftM)
import           Data.Char                     (digitToInt)
import           Text.Parsec.Prim              (Parsec)
import           Text.ParserCombinators.Parsec (many1, oneOf)

import           KMC.Syntax.ParserCombinators  (exactly)
import           KMC.Syntax.ParserTypes        (Parser)

-- | Supported bases for integers.  Most of them are silly.
data IntegerBase = Unary    -- ^ The original and best
                 | Binary
                 | Ternary
                 | Octal
                 | Decimal
                 | Hexadecimal
    deriving (Show, Eq)

-- | Parse a non-negative numeral in the given base and,
--   if specified, with the given length.
numeralP :: IntegerBase -> (Maybe Int) -> Parser Int
numeralP base len = liftM combine $ repetitions len
        (liftM digitToInt (oneOf digits))
    where
    repetitions Nothing  = many1
    repetitions (Just n) = exactly n
    (digits, mult) = case base of
            Unary       -> ("1", 1)
            Binary      -> ("01", 2)
            Ternary     -> ("012", 3)
            Octal       -> (['0'..'7'], 8)
            Decimal     -> (['0'..'9'], 10)
            Hexadecimal -> (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'], 16)
    combine = snd . foldr (\d (pos, acc) -> (mult * pos, acc + pos * d)) (1, 0)

