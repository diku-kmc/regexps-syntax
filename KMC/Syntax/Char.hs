module KMC.Syntax.Char where

import           Data.Char                          (toLower, toUpper)
import           Text.Parsec.Prim                   (try, (<?>), (<|>))
import           Text.ParserCombinators.Parsec.Char (char)

import           KMC.Syntax.ParserTypes

-- | Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- | Match an any-cased version of a string.
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

