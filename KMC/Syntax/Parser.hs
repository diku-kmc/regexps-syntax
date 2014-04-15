module KMC.Syntax.Parser (parseRegex) where

import           Control.Applicative           (pure, (*>), (<$), (<$>), (<*),
                                                (<*>), (<|>))
import           Control.Monad                 (liftM)
import           Data.Char                     (digitToInt)
import           Data.Functor.Identity         (Identity)
import           Text.Parsec.Expr              (Assoc (..), Operator (..),
                                                OperatorTable,
                                                buildExpressionParser)
import           Text.ParserCombinators.Parsec hiding (Parser, (<|>))

import           KMC.Syntax.External
import           KMC.Syntax.ParserCombinators
import           KMC.Syntax.ParserTypes
import           KMC.Syntax.Unicode

-- | Parse a regular expression or fail with an error message.
parseRegex :: String -- ^ Input string
           -> Either String (Anchoring, ParsedRegex) -- ^ Error message or parse result.
parseRegex str = case parse anchoredRegexP "-" str of
                   Left e   -> Left $ show e
                   Right re -> Right re


anchoredRegexP :: Parser (Anchoring, ParsedRegex)
anchoredRegexP = do
    anchorStart <- (char '^' >> return True) <|> (return False)
    re <- regexP
    anchorEnd <- (char '$' >> return True) <|> (return False)
    return $ flip (,) re $ case (anchorStart, anchorEnd) of
        (True, True) -> AnchorBoth
        (True, _   ) -> AnchorStart
        (_   , True) -> AnchorEnd
        _            -> AnchorNone

-- | The main regexp parser.
regexP :: Parser ParsedRegex
regexP = buildExpressionParser table $
                parens regexP
            <|> posixNamedSetP
            <|> brackets classP
            <|> suppressDelims suppressedP
            <|> wildcardP
            <|> charP

-- | The operator table, defining how subexpressions are glued together with
--   the operators, along with their fixity and associativity information.
table :: OperatorTable String () Identity ParsedRegex
table = [
        -- The various postfix operators bind tightest.
          map Postfix [ -- Lazy and greedy Kleene Star:
                        try (string "*?")     >>  return LazyStar
                      , char '*'              >>  return Star
                      -- Lazy and greedy ?-operator (1 or 0 repetitions):
                      , try (string "??")     >>  return LazyQuestion
                      , char '?'              >>  return Question
                      -- Lazy and greedy +-operator (1 or more repetitions):
                      , try (string "+?")     >>  return LazyPlus
                      , char '+'              >>  return Plus
                      -- Lazy and greedy range expressions:
                      , try (braces rangeP <* char '?')
                            >>= \(n, m) -> return (\e -> LazyRange e n m)
                      , braces (rangeP)
                            >>= \(n, m) -> return (\e -> Range e n m)
                      ]
        -- Product (juxtaposition) binds tigther than sum.
        , [ Infix (notFollowedBy (char '|') >> return Concat) AssocRight ]
        -- Sum binds least tight.
        , [ Infix (char '|'                 >> return Branch) AssocRight ]
        ]


-- | Parse a regular expression and suppress it.
suppressedP :: Parser ParsedRegex
suppressedP = Suppress <$> regexP

-- | Parse a dot, returning a constructor representing the "wildcard symbol".
--   This must be added to the datatype Regex before it can be used.
wildcardP :: Parser ParsedRegex
wildcardP = Dot <$ char '.'

-- | Parse a single character and build a Regex for it.
charP :: Parser ParsedRegex
charP = Chr <$> legalChar

-- | Parse a legal character.
legalChar :: Parser Char
legalChar = noneOf notChars
         -- <|> try (char '$' <* lookAhead anyToken)
         <|> try (char '\\' *> (u <$> oneOf (map fst cs)))
         <|> unicodeCodePointP
  where cs = [('n', '\n'), ('t', '\t'), ('r', '\r')] ++
              zip notChars notChars
        notChars = ".\\" ++ "()" ++ "[*?+$^{|"
        u c = let Just x = lookup c cs in x




--evil = "\\$\\(\\)\\*\\+\\.\\?\\[\\\\\\^\\{\\|"
--evilM = "$()*+.?[\\^{|"

-- | Parse a range of numbers.  For n, m natural numbers:
--  'n'   - "n repetitions" - (n, Just n)
--  'n,'  - "n or more repetitions" - (n, Nothing)
--  'n,m' - "between n and m repetitions" - (n, Just m)
rangeP :: Parser (Int, Maybe Int)
rangeP = do
    n <- naturalP
    ((,) n) <$> (char ',' *> optionMaybe naturalP
              <|> pure (Just n))

-- | Parse a decimal number
naturalP :: Parser Int
naturalP = numeralP Decimal Nothing



-- | Parse a character class.  A character class consists of a sequence of
--   ranges: [a-ctx-z] is the range [(a,c), (t,t), (x,z)].  If the first symbol
--   is a caret ^, the character class is negative, i.e., it specifies all
--   symbols *not* in the given ranges.
classP :: Parser ParsedRegex
classP = Class <$> ((False <$ char '^') <|> pure True)
            <*> ((:) <$> charClassP True <*> many (charClassP False))
    where
    charClassP isFirst = do
        c1 <- if isFirst then anyChar else noneOf "]"
        try (char '-' >> ((,) c1) <$> noneOf "]")
            <|> pure (c1, c1)

-- FIXME: What's the difference between NamedSet True and NamedSet False?
posixNamedSetP :: Parser ParsedRegex
posixNamedSetP = parseTable
    (try . delims "[:" ":]" . string) (NamedSet True)
    [ ("alnum", NSAlnum)
    , ("alpha", NSAlpha)
    , ("ascii", NSAscii)
    , ("blank", NSBlank)
    , ("cntrl", NSCntrl)
    , ("digit", NSDigit)
    , ("graph", NSGraph)
    , ("lower", NSLower)
    , ("print", NSPrint)
    , ("punct", NSPunct)
    , ("space", NSSpace)
    , ("upper", NSUpper)
    , ("word",  NSWord)
    , ("digit", NSXDigit)
    ]

