module KMC.Syntax.Parser (parseRegex) where

import           Control.Applicative           (pure, (*>), (<$), (<$>), (<*),
                                                (<*>), (<|>))
import           Control.Monad                 (liftM)
import           Data.Char                     (digitToInt)
import           Data.Functor.Identity         (Identity)
import           Text.Parsec.Expr              (Assoc (..), Operator (..),
                                                OperatorTable,
                                                buildExpressionParser)
import           Text.Parsec.Prim              (parserZero)
import           Text.ParserCombinators.Parsec hiding (Parser, (<|>))

import           KMC.Syntax.Config
import           KMC.Syntax.External
import           KMC.Syntax.Numeral
import           KMC.Syntax.ParserCombinators
import           KMC.Syntax.ParserTypes
import           KMC.Syntax.Unicode

-- | Parse a regular expression or fail with an error message.
parseRegex :: RegexParserConfig
           -> String -- ^ Input string
           -> Either String (Anchoring, ParsedRegex) -- ^ Error message or parse result.
parseRegex conf str = case parse (anchoredRegexP conf) "-" str of
                   Left e   -> Left $ show e
                   Right re -> Right re


anchoredRegexP :: RegexParserConfig -> Parser (Anchoring, ParsedRegex)
anchoredRegexP conf = do
    anStart <- anchorStart
    re <- regexP conf
    anEnd <- anchorEnd
    return $ flip (,) re $ case (anStart, anEnd) of
        (True, True) -> AnchorBoth
        (True, _   ) -> AnchorStart
        (_   , True) -> AnchorEnd
        _            -> AnchorNone
  where
    (anchorStart, anchorEnd) = if rep_anchoring conf
                  then ((char '^' >> return True) <|> (return False),
                        (char '$' >> return True) <|> (return False))
                  else (return False, return False)

-- | The main regexp parser.
regexP :: RegexParserConfig -> Parser ParsedRegex
regexP conf = buildExpressionParser (table conf) $
               ifElseP (rep_grouping conf)
                      (  (nonGroupParens (regexP conf) >>= return . Group False)
                     <|> (parens (regexP conf) >>= return . Group True))
                      (  parens (regexP conf) >>= return . Group False)
           <|> ifP (rep_posix_names conf) posixNamedSetP
           <|> ifP (rep_charclass conf) (brackets classP)
           <|> ifP (rep_suppression conf) (suppressDelims (suppressedP conf))
           <|> ifP (rep_wildcard conf) wildcardP
           <|> charP conf

ifP :: Bool -> Parser a -> Parser a
ifP b p = ifElseP b p parserZero

ifElseP :: Bool -> Parser a -> Parser a -> Parser a
ifElseP True  t _ = t
ifElseP False _ f = f

-- | A "conditional cons" operator.  It conses an element like the normal cons
--   if the boolean is true, otherwise it leaves the list intact.
(?:) :: (Bool, a) -> [a] -> [a]
(?:) (True,  x) = (:) x
(?:) (False, _) = id
infixr 5 ?: -- Same fixity as (:)

-- | The operator table, defining how subexpressions are glued together with
--   the operators, along with their fixity and associativity information.
table :: RegexParserConfig -> OperatorTable String () Identity ParsedRegex
table conf = [
          -- The various postfix operators (which are determined by the
          -- configuration object) bind tightest.
          map Postfix $
            -- Lazy and greedy Kleene Star:
            (rep_lazyness conf, try (string "*?") >> return LazyStar)         ?:
            (char '*' >> return Star)                                          :
            -- Lazy and greedy ?-operator (1 or 0 repetitions):
            (rep_lazyness conf && rep_question conf,
                try (string "??") >> return LazyQuestion)                     ?:
            (rep_question conf, char '?' >>  return Question)                 ?:
            -- Lazy and greedy +-operator (1 or more repetitions):
            (rep_lazyness conf && rep_plus conf,
                try (string "+?") >> return LazyPlus)                         ?:
            (rep_plus conf, char '+' >>  return Plus)                         ?:
            -- Lazy and greedy range expressions:
            (rep_lazyness conf && rep_ranges conf,
                try (braces rangeP <* char '?')
                    >>= \(n, m) -> return (\e -> LazyRange e n m))            ?:
            (rep_ranges conf,
                braces (rangeP)
                    >>= \(n, m) -> return (\e -> Range e n m))                ?:
                                                                              []
        -- Product (juxtaposition) binds tigther than sum.
        , [ Infix (notFollowedBy (char '|') >> return Concat) AssocRight ]
        -- Sum binds least tight.
        , [ Infix (char '|'                 >> return Branch) AssocRight ]
        ]


-- | Parse a regular expression and suppress it.
suppressedP :: RegexParserConfig -> Parser ParsedRegex
suppressedP conf = Suppress <$> regexP conf

-- | Parse a dot, returning a constructor representing the "wildcard symbol".
--   This must be added to the datatype Regex before it can be used.
wildcardP :: Parser ParsedRegex
wildcardP = Dot <$ char '.'

-- | Parse a single character and build a Regex for it.
charP :: RegexParserConfig -> Parser ParsedRegex
charP conf = Chr <$> legalChar conf

-- | Parse a legal character.
legalChar :: RegexParserConfig -> Parser Char
legalChar conf = noneOf notChars
         -- <|> try (char '$' <* lookAhead anyToken) -- ????
         <|> try (char '\\' *> (u <$> oneOf (map fst cs)))
         <|> ifP (rep_unicode conf) unicodeCodePointP
  where cs = [('n', '\n'), ('t', '\t'), ('r', '\r')] ++
              zip notChars notChars
        notChars = "*|()\\" ++ ( -- These are always special
                    (rep_wildcard conf,  '.') ?: -- All these are special
                    (rep_anchoring conf, '$') ?: -- on the condition that
                    (rep_anchoring conf, '^') ?: -- their superpowers have
                    (rep_charclass conf, '[') ?: -- been "unlocked".
                    (rep_question conf,  '?') ?:
                    (rep_plus conf,      '+') ?:
                    (rep_ranges conf,    '{') ?: [] )
        u c = let Just x = lookup c cs in x


-- | Parse a range of numbers.  For n, m natural numbers:
--  'n'   - "n repetitions" - (n, Just n)
--  'n,'  - "n or more repetitions" - (n, Nothing)
--  'n,m' - "between n and m repetitions" - (n, Just m)
rangeP :: Parser (Int, Maybe Int)
rangeP = do
    n <- numeralP Decimal Nothing
    ((,) n) <$> (char ',' *> optionMaybe (numeralP Decimal Nothing)
              <|> pure (Just n))


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

