module KMC.Syntax.Numeral
    ( IntegerBase(..)
    , numeralP
    ) where

import           Control.Monad                 (liftM)
import           Data.Char                     (digitToInt)
import           Text.ParserCombinators.Parsec (many, oneOf)

import           KMC.Syntax.ParserCombinators  (repetitions)
import           KMC.Syntax.ParserTypes        (Parser)

-- | Supported bases for integers.  Most of them are silly.
data IntegerBase = Unary    -- ^ The original and best
                 | Binary
                 | Ternary
                 | Octal
                 | Decimal
                 | Hexadecimal
    deriving (Show)

type IntegerWidth = Maybe (Ordering, Int)

-- | Parse a non-negative numeral in the given base and,
--   if specified, with the given length.
numeralP :: IntegerBase -> IntegerWidth -> Parser Int
numeralP base len = liftM combine $ repeats len
        (liftM digitToInt (oneOf digits))
    where
    repeats Nothing       = many
    repeats (Just (o, n)) = repetitions o n
    (digits, mult) = case base of
            Unary       -> ("1", 1)
            Binary      -> ("01", 2)
            Ternary     -> ("012", 3)
            Octal       -> (['0'..'7'], 8)
            Decimal     -> (['0'..'9'], 10)
            Hexadecimal -> (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'], 16)
    combine = snd . foldr (\d (pos, acc) -> (mult * pos, acc + pos * d)) (1, 0)

