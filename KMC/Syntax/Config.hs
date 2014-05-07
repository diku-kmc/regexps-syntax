module KMC.Syntax.Config where

-- | Configuration data for a regex parser.
data RegexParserConfig = RegexParserConfig
            { rep_posix_names   :: Bool -- ^ Use posix named sets?
            , rep_unicode       :: Bool -- ^ Unicode support
            , rep_lazyness      :: Bool -- ^ Lazy operators
            , rep_ranges        :: Bool -- ^ Range expressions
            , rep_plus          :: Bool -- ^ The plus operator
            , rep_question      :: Bool -- ^ The question-mark operator
            , rep_suppression   :: Bool -- ^ Suppression support
            , rep_wildcard      :: Bool -- ^ Wildcard symbol
            , rep_charclass     :: Bool -- ^ Character classes
            , rep_anchoring     :: Bool -- ^ Anchoring support
            , rep_grouping      :: Bool -- ^ Use (non-)grouping parens
            , rep_freespacing   :: Bool -- ^ Free-spacing mode
            , rep_illegal_chars :: [Char] -- ^ Extra illegal (escapable) chars.
            , rep_with_unit     :: Bool  -- ^ Parse "1" as unit?
            }
    deriving (Show)

-- | A basic regex parser doesn't have any of the fancy stuff.
basicRegexParser :: RegexParserConfig
basicRegexParser = RegexParserConfig
                    { rep_posix_names   = False
                    , rep_unicode       = False
                    , rep_lazyness      = False
                    , rep_ranges        = False
                    , rep_plus          = False
                    , rep_question      = False
                    , rep_suppression   = False
                    , rep_wildcard      = False
                    , rep_charclass     = False
                    , rep_anchoring     = False
                    , rep_grouping      = False
                    , rep_freespacing   = False
                    , rep_illegal_chars = []
                    , rep_with_unit     = False
                    }

-- | A fancy regex parser has all the fancy stuff.
fancyRegexParser :: RegexParserConfig
fancyRegexParser = RegexParserConfig
                    { rep_posix_names   = True
                    , rep_unicode       = True
                    , rep_lazyness      = True
                    , rep_ranges        = True
                    , rep_plus          = True
                    , rep_question      = True
                    , rep_suppression   = True
                    , rep_wildcard      = True
                    , rep_charclass     = True
                    , rep_anchoring     = True
                    , rep_grouping      = True
                    , rep_freespacing   = True
                    , rep_illegal_chars = []
                    , rep_with_unit     = False
                    }
