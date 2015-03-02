module KMC.Syntax.Unicode where

import           Control.Applicative           ((*>))
import           Data.Char                     (chr)
import           Text.ParserCombinators.Parsec (char, string, try,
                                                (<|>))

import           KMC.Syntax.Numeral
import           KMC.Syntax.ParserCombinators
import           KMC.Syntax.ParserTypes

-- | Parse a Unicode code point specification of a char.
unicodeCodePointP :: Parser Char
unicodeCodePointP = char '\\' *> (char 'u' *> numeralP Hexadecimal (Just (EQ, 4)))
                >>= return . chr


unicodeScriptP :: Parser UnicodeScript
unicodeScriptP = genParseTable
                (\s -> try $ string "\\p" *> braces (string s))
                id
                [Arabic .. Yi]

unicodeBlockP :: Parser UnicodeBlock
unicodeBlockP = genParseTable
                (\s -> try $ string "\\p" *> braces (string (fix_name s)))
                id
                [InAlphabetic_Presentation_Forms .. InYijing_Hexagram_Symbols]
    where
    fix_name s = case s of
        "InArabic_Presentation_Forms_A" -> "InArabic_Presentation_Forms-A"
        "InArabic_Presentation_Forms_B" -> "InArabic_Presentation_Forms-B"
        "InMiscellaneous_Mathematical_Symbols_A" -> "InMiscellaneous_Mathematical_Symbols-A"
        "InMiscellaneous_Mathematical_Symbols_B" -> "InMiscellaneous_Mathematical_Symbols-B"
        "InSupplemental_Arrows_A" -> "InSupplemental_Arrows-A"
        "InSupplemental_Arrows_B" -> "InSupplemental_Arrows-B"
        "InLatin_Extended_A" -> "InLatin_Extended-A"
        "InLatin_Extended_B" -> "InLatin_Extended-B"
        x -> x


-- | The Unicode scipt possibilities.
data UnicodeScript = Arabic
                   | Common
                   | Armenian
                   | Bengali
                   | Bopomofo
                   | Braille
                   | Buhid
                   | CanadianAboriginal
                   | Cherokee
                   | Cyrillic
                   | Devanagari
                   | Ethiopic
                   | Georgian
                   | Greek
                   | Gujarati
                   | Gurmukhi
                   | Han
                   | Hangul
                   | Hanunoo
                   | Hebrew
                   | Hiragana
                   | Inherited
                   | Kannada
                   | Katakana
                   | Khmer
                   | Lao
                   | Latin
                   | Limbu
                   | Malayalam
                   | Mongolian
                   | Myanmar
                   | Ogham
                   | Oriya
                   | Runic
                   | Sinhala
                   | Syriac
                   | Tagalog
                   | Tagbanwa
                   | TaiLe
                   | Tamil
                   | Telugu
                   | Thaana
                   | Thai
                   | Tibetan
                   | Yi
    deriving (Show, Eq, Enum)

-- | All the "blocks" that unicode is divided into.
data UnicodeBlock = InAlphabetic_Presentation_Forms
                  | InArabic
                  | InArabic_Presentation_Forms_A
                  | InArabic_Presentation_Forms_B
                  | InArmenian
                  | InArrows
                  | InBasic_Latin
                  | InBengali
                  | InBlock_Elements
                  | InBopomofo
                  | InBopomofo_Extended
                  | InBox_Drawing
                  | InBraille_Patterns
                  | InBuhid
                  | InCherokee
                  | InCJK_Compatibility
                  | InCJK_Compatibility_Forms
                  | InCJK_Compatibility_Ideographs
                  | InCJK_Radicals_Supplement
                  | InCJK_Symbols_and_Punctuation
                  | InCJK_Unified_Ideographs
                  | InCJK_Unified_Ideographs_Extension_A
                  | InCombining_Diacritical_Marks
                  | InCombining_Diacritical_Marks_for_Symbols
                  | InCombining_Half_Marks
                  | InControl_Pictures
                  | InCurrency_Symbols
                  | InCyrillic
                  | InCyrillic_Supplementary
                  | InDevanagari
                  | InDingbats
                  | InEnclosed_Alphanumerics
                  | InEnclosed_CJK_Letters_and_Months
                  | InEthiopic
                  | InGeneral_Punctuation
                  | InGeometric_Shapes
                  | InGeorgian
                  | InGreek_and_Coptic
                  | InGreek_Extended
                  | InGujarati
                  | InGurmukhi
                  | InHalfwidth_and_Fullwidth_Forms
                  | InHangul_Compatibility_Jamo
                  | InHangul_Jamo
                  | InHangul_Syllables
                  | InHanunoo
                  | InHebrew
                  | InHigh_Private_Use_Surrogates
                  | InHigh_Surrogates
                  | InHiragana
                  | InIdeographic_Description_Characters
                  | InIPA_Extensions
                  | InKanbun
                  | InKangxi_Radicals
                  | InKannada
                  | InKatakana
                  | InKatakana_Phonetic_Extensions
                  | InKhmer
                  | InKhmer_Symbols
                  | InLao
                  | InLatin_1_Supplement
                  | InLatin_Extended_A
                  | InLatin_Extended_B
                  | InLatin_Extended_Additional
                  | InLetterlike_Symbols
                  | InLimbu
                  | InLow_Surrogates
                  | InMalayalam
                  | InMathematical_Operators
                  | InMiscellaneous_Mathematical_Symbols_A
                  | InMiscellaneous_Mathematical_Symbols_B
                  | InMiscellaneous_Symbols
                  | InMiscellaneous_Symbols_and_Arrows
                  | InMiscellaneous_Technical
                  | InMongolian
                  | InMyanmar
                  | InNumber_Forms
                  | InOgham
                  | InOptical_Character_Recognition
                  | InOriya
                  | InPhonetic_Extensions
                  | InPrivate_Use_Area
                  | InRunic
                  | InSinhala
                  | InSmall_Form_Variants
                  | InSpacing_Modifier_Letters
                  | InSpecials
                  | InSuperscripts_and_Subscripts
                  | InSupplemental_Arrows_A
                  | InSupplemental_Arrows_B
                  | InSupplemental_Mathematical_Operators
                  | InSyriac
                  | InTagalog
                  | InTagbanwa
                  | InTai_Le
                  | InTamil
                  | InTelugu
                  | InThaana
                  | InThai
                  | InTibetan
                  | InUnified_Canadian_Aboriginal_Syllabics
                  | InVariation_Selectors
                  | InYi_Radicals
                  | InYi_Syllables
                  | InYijing_Hexagram_Symbols
    deriving (Show, Eq, Enum)
