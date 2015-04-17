module KMC.Syntax.Parser (parseRegex, anchoredRegexP) where

import           Control.Applicative           (pure, (*>), (<$), (<$>), (<*),
                                                (<*>), (<|>))
import           Data.Char                     (chr)
import           Data.Functor.Identity         (Identity)
import           Text.Parsec.Expr              (Assoc (..), Operator (..),
                                                OperatorTable,
                                                buildExpressionParser)
import           Text.Parsec.Prim              (parserZero)
import           Text.ParserCombinators.Parsec hiding (Parser, (<|>))
import           Text.Printf                   (printf)

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

-- | Wraps a parser in a parser that throws away whitespace and
-- comments if the configuration object tells it to.
freespaced :: RegexParserConfig -> Parser a -> Parser a
freespaced conf p = if rep_freespacing conf
                    then spacesAndCommentsP *> p <* spacesAndCommentsP
                    else p

anchoredRegexP :: RegexParserConfig -> Parser (Anchoring, ParsedRegex)
anchoredRegexP conf = freespaced conf $ do
    let conf' = if rep_with_unit conf
                then conf { rep_illegal_chars = '1' : rep_illegal_chars conf}
                else conf
    anStart <- anchorStart
    re <- regexP conf'
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
regexP conf = freespaced conf $ buildExpressionParser (table conf) $
               ifElseP (rep_grouping conf)
                      (  (nonGroupParens (regexP conf) >>= return . Group False)
                     <|> (parens (regexP conf) >>= return . Group True))
                      (  parens (regexP conf) >>= return . Group False)
           <|> ifP (rep_posix_names conf) (freespaced conf posixNamedSetP)
           <|> ifP (rep_charclass conf) (brackets (classP conf))
           <|> ifP (rep_suppression conf) (suppressDelims (suppressedP conf))
           <|> ifP (rep_wildcard conf) (freespaced conf wildcardP)
           <|> ifP (rep_with_unit conf) (freespaced conf unitP)
           <|> (freespaced conf $ charP NoCC conf)

-- | Throw away whitespace and comments.
spacesAndCommentsP :: Parser ()
spacesAndCommentsP = spaces
                    >> optional (
                        char '#' >>
                        manyTill anyChar (eof <|> (newline >> return ())) >>
                        spaces)

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
          map Postfix $ map (freespaced conf) $
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
        , [ Infix (freespaced conf (notFollowedBy (char '|') >> return Concat)) AssocRight ]
        -- Sum binds least tight.
        , [ Infix (freespaced conf (char '|'                 >> return Branch)) AssocRight ]
        ]


-- | Parse a regular expression and suppress it.
suppressedP :: RegexParserConfig -> Parser ParsedRegex
suppressedP conf = Suppress <$> regexP conf

-- | Parse a dot, returning a constructor representing the "wildcard symbol".
--   This must be added to the datatype Regex before it can be used.
wildcardP :: Parser ParsedRegex
wildcardP = Dot <$ char '.'

-- | If the parser is configured to recognize unit, this parser will be in
unitP :: Parser ParsedRegex
unitP = One <$ char '1'

-- | Parse a single character and build a Regex for it.
charP :: CharClassPos -> RegexParserConfig -> Parser ParsedRegex
charP ccp conf = Chr <$> legalChar ccp conf

-- | Signal whether the character to be parsed is outside of a character class,
--   or if it is inside one, whether it is the first or not.
data CharClassPos = NoCC | FirstInCC | InCC

-- | Parse a legal character.  Characters can be specificed directly as byte values
-- using the syntax \xFF where FF is the byte in hexadecimal notation, or the syntax
-- \x{F...} where at least one hexadecimal digit is given between the braces.
legalChar :: CharClassPos -> RegexParserConfig -> Parser Char
legalChar ccp conf = try (char '\\' *> ( u <$> oneOf (map fst cs)
                                        <|> char 'x' *> (namedByte <|> namedByteSequence)))
                  <|> try (ifP (rep_unicode conf) unicodeCodePointP)
                  <|> noneOf notChars
  where cs = [ ('n', '\n'), ('t', '\t'), ('r', '\r'),
               ('a', '\a'), ('f', '\f'), ('v', '\v') ] ++
              zip notChars notChars
        notChars = (if inCC then ""  -- These are always special
                            else "*|()\\" ++ rep_illegal_chars conf)
                ++ (
                (outofCC && rep_wildcard conf,    '.') ?: -- All these are special
                (outofCC && rep_anchoring conf,   '$') ?: -- on the condition that
                (outofCC && rep_anchoring conf,   '^') ?: -- their superpowers have
                (outofCC && rep_charclass conf,   '[') ?: -- been "unlocked" by the
                (outofCC && rep_question conf,    '?') ?: -- RegexParserConfig.
                (outofCC && rep_plus conf,        '+') ?: --
                (outofCC && rep_ranges conf,      '{') ?: --
                (outofCC && rep_freespacing conf, '#') ?: --
                (inCC && not inCCFirst,           ']') ?: [] )
        u c = let Just x = lookup c cs in x
        inCC = not outofCC
        outofCC = case ccp of
                  NoCC -> True
                  _    -> False
        inCCFirst = case ccp of
                  FirstInCC -> True
                  _         -> False

-- | Parse a string of the form "FF" into the char with byte value FF.
namedByte :: Parser Char
namedByte = chr <$> numeralP Hexadecimal (Just (EQ, 2))

-- | Parse a string of the form "{ABCD...}" into a char with the given hex code.
namedByteSequence :: Parser Char
namedByteSequence =  chr <$>
                     braces (numeralP Hexadecimal (Just (GT, 1)))

-- | Parse a range of numbers.  For n, m natural numbers:
--  'n'   - "n repetitions" - (n, Just n)
--  'n,'  - "n or more repetitions" - (n, Nothing)
--  'n,m' - "between n and m repetitions" - (n, Just m)
rangeP :: Parser (Int, Maybe Int)
rangeP = do
    n <- numeralP Decimal Nothing
    ((,) n) <$> (char ',' *> optionMaybe (numeralP Decimal (Just (GT, 1)))
              <|> pure (Just n))

-- | Parse a character class.  A character class consists of a sequence of
--   ranges: [a-ctx-z] is the range [(a,c), (t,t), (x,z)].  If the first symbol
--   is a caret ^, the character class is negative, i.e., it specifies all
--   symbols *not* in the given ranges.
classP :: RegexParserConfig -> Parser ParsedRegex
classP conf = Class <$> ((False <$ char '^') <|> pure True)
            <*> ((:) <$> charClassP True <*> many (charClassP False))
    where
    charClassP isFirst = do
        c1 <- ifElseP isFirst (legalChar FirstInCC conf) (legalChar InCC conf)
        try (char '-' >> ((,) c1) <$> (legalChar InCC conf))
            <|> pure (c1, c1)


-- From the sed manual:
-- Though character classes don't generally conserve space on the line, they help make scripts portable for international use. The equivalent character sets /for U.S. users/ follows:
--      [[:alnum:]]  - [A-Za-z0-9]     Alphanumeric characters
--      [[:alpha:]]  - [A-Za-z]        Alphabetic characters
--      [[:blank:]]  - [ \x09]         Space or tab characters only
--      [[:cntrl:]]  - [\x00-\x19\x7F] Control characters
--      [[:digit:]]  - [0-9]           Numeric characters
--      [[:graph:]]  - [!-~]           Printable and visible characters
--      [[:lower:]]  - [a-z]           Lower-case alphabetic characters
--      [[:print:]]  - [ -~]           Printable (non-Control) characters
--      [[:punct:]]  - [!-/:-@[-`{-~]  Punctuation characters
--      [[:space:]]  - [ \t\v\f]       All whitespace chars
--      [[:upper:]]  - [A-Z]           Upper-case alphabetic characters
--      [[:xdigit:]] - [0-9a-fA-F]     Hexadecimal digit characters

-- FIXME: What's the difference between NamedSet True and NamedSet False?
posixNamedSetP :: Parser ParsedRegex
posixNamedSetP = parseTable
    (try . delims "[[:" ":]]" . string) (NamedSet True)
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

