module KMC.Syntax.ParserCombinators
    ( parsersFromTable
    , exactly
    , delims
    , parens
    , brackets
    , braces
    , suppressDelims
    , parseTable
    , genParseTable
    ) where

import           Control.Applicative           ((*>), (<*))
import           Text.ParserCombinators.Parsec (string, try, choice)

import           KMC.Syntax.ParserTypes        (Parser)

--------------------------------------------------------------------------------
-- Various useful parser combinators.
--------------------------------------------------------------------------------

-- | Automatically build a parse table from a data type that implements the Show
--   type class.  Takes a function that specifies how the parsers
--   should behave as a function of their "Show value", and a list of the
--   constructors that should be parsed.
genParseTable :: (Show t) => (String -> Parser String)
              -> [t] -> Parser t
genParseTable p = choice . parsersFromTable p id . map (\v -> (show v, v))

parseTable :: (a -> Parser a) -> (b -> c) -> [(a, b)] -> Parser c
parseTable parserMod valMod = choice . parsersFromTable parserMod valMod

-- | Construct a list of parsers from a lookup table.  The result of each parser
--   is the value created by "valMod" applied to the right value in the tuple.
parsersFromTable :: (a -> Parser a) -> (b -> c) -> [(a, b)] -> [Parser c]
parsersFromTable parserMod valMod = map
    (\(s,v) -> parserMod s >> return (valMod v))

-- | Repeat the given parser exactly n times and collect the results.
exactly :: Int -> Parser a -> Parser [a]
exactly 0 _ = return []
exactly 1 p = p >>= return . (:[])
exactly n p = do
    x <- p
    xs <- exactly (n - 1) p
    return (x : xs)


-- | Build a parser that parses the given left- and right-delimiters around
--   the provided parser p.
delims :: String -> String -> Parser a -> Parser a
delims left right p = try (string left) *> p <* string right

-- | Put parentheses around parser
parens :: Parser a -> Parser a
parens = delims "(" ")"

-- | Put brackets around parser
brackets :: Parser a -> Parser a
brackets = delims "[" "]"

-- | Put braces around parser
braces :: Parser a -> Parser a
braces = delims "{" "}"

-- | Put "suppression delimiters" around parser
suppressDelims :: Parser a -> Parser a
suppressDelims = delims "$(" ")$"
