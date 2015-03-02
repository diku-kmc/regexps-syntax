module KMC.Syntax.ParserTypes
    ( ParsedRegex
    , Parser
    ) where

import           Text.Parsec.Prim (Parsec)

import           KMC.Syntax.External (Regex)

type ParsedRegex = Regex
type Parser = Parsec String ()


