module KMC.Syntax.ParserCombinators
    ( repetitions
    , delims
    , parens
    , brackets
    , braces
    , suppressDelims
    , parseTable
    , nonGroupParens
    , genParseTable
    ) where

import           Control.Applicative           ((*>), (<*))
import           Text.ParserCombinators.Parsec (choice, count, many, many1,
                                                optionMaybe, string, try)
    
import           KMC.Syntax.ParserTypes        

--------------------------------------------------------------------------------
-- Various useful parser combinators.
--------------------------------------------------------------------------------




-- | Automatically build a parse table from a data type that implements the Show
--   type class.  Takes a function that specifies how the parsers
--   should behave as a function of their "Show value", a function to transform
--   the parsed constructors, and a list of the constructors that should be
--   made parsable by what "show" returns.
genParseTable :: (Show a)
              => (String -> Parser String) -> (a -> b) -> [a] -> Parser b
genParseTable p f = parseTable p f . map (\v -> (show v, v))

-- | Build a parser for a given table.  The result of each parser
--   is the value created by "valMod" applied to the right value in the tuple.
parseTable :: (s -> Parser s) -> (a -> b) -> [(s, a)] -> Parser b
parseTable parserMod valMod = choice . map
    (\(s,v) -> parserMod s >> return (valMod v))

-- | Given an ordering relation and an integer n, repeat the given parser either
--   (== n) times, (<= n) times, or (>= n) times.  Negative n are clamped to 0.
repetitions :: Ordering -> Int -> Parser a -> Parser [a]
repetitions o n = let n' = if n < 0 then 0 else n in case o of
                    EQ -> count n'
                    LT -> maximumRepetitions n'
                    GT -> minimumRepetitions n'

-- | Repeat parser minimum n times and collect the results.
minimumRepetitions :: Int -> Parser a -> Parser [a]
minimumRepetitions 0 p = many p
minimumRepetitions 1 p = many1 p
minimumRepetitions n p = do
    xs <- count n p
    xs' <- many p
    return (xs ++ xs')

-- | Repeat parser maximum n times and collect the results.
maximumRepetitions :: Int -> Parser a -> Parser [a]
maximumRepetitions 0 _ = return []
maximumRepetitions 1 p = p >>= return . (:[])
maximumRepetitions n p = do
    x <- p
    mxs <- optionMaybe (maximumRepetitions (n - 1) p)
    case mxs of
        Nothing -> return [x]
        Just xs -> return (x:xs)

-- | Build a parser that parses the given left- and right-delimiters around
--   the provided parser p.
delims :: String -> String -> Parser a -> Parser a
delims left right p = try (string left) *> p <* (string right)

-- | Put parentheses around parser
parens :: Parser a -> Parser a
parens = delims "(" ")"

nonGroupParens :: Parser a -> Parser a
nonGroupParens = delims "(?:" ")"

-- | Put brackets around parser
brackets :: Parser a -> Parser a
brackets = delims "[" "]"

-- | Put braces around parser
braces :: Parser a -> Parser a
braces = delims "{" "}"

-- | Put "suppression delimiters" around parser
suppressDelims :: Parser a -> Parser a
suppressDelims = delims "$(" ")$"
