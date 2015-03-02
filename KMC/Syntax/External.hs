module KMC.Syntax.External where

import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       (second)
import           Data.Char           (chr, ord)
import           Data.List           (sort)
import           Data.Maybe          (fromMaybe, isNothing)

import qualified KMC.Syntax.Internal as Int

data Anchoring = AnchorNone
               | AnchorStart
               | AnchorEnd
               | AnchorBoth
  deriving (Show)

-- | Named character classes from the POSIX standard.
data POSIXNamedSet = NSAlnum
                   | NSAlpha
                   | NSAscii
                   | NSBlank
                   | NSCntrl
                   | NSDigit
                   | NSGraph
                   | NSLower
                   | NSPrint
                   | NSPunct
                   | NSSpace
                   | NSUpper
                   | NSWord
                   | NSXDigit
  deriving (Eq,Ord,Show)

data Regex = One  -- ^ Epsilon
           | Dot  -- ^ Wildcard symbol
           | Chr Char   -- ^ Single character
           | Group Bool Regex -- ^ Group (parentheses)
           | Concat Regex Regex -- ^ Concatenation
           | Branch Regex Regex -- ^ Sum (alternation)
           | Class Bool [(Char, Char)] -- ^ Character class.  False indicates
                                       -- the class of symbols *not* mentioned.
           | NamedSet Bool POSIXNamedSet -- ^ POSIX named character class
           | Range Regex Int (Maybe Int) -- ^ Range expression.  "n Just m" is
                                         -- repetitition between n and m times,
                                         -- and "n Nothing" is n or more times.
           | LazyRange Regex Int (Maybe Int)
           | Star Regex -- ^ Kleene star
           | LazyStar Regex
           | Plus Regex -- ^ Plus expression (1 or more repetitions)
           | LazyPlus Regex
           | Question Regex -- ^ Question mark (1 or 0 repetitions)
           | LazyQuestion Regex
           | Suppress Regex
  deriving (Show, Eq, Ord)

data OpAssoc = OpAssocLeft | OpAssocRight | OpAssocNone
  deriving (Eq, Ord, Show)

unparse :: Regex -> String
unparse = go OpAssocNone (0::Int)
    where
      noncap s = "(?:" ++ s ++ ")"
      cap s = "(" ++ s ++ ")"

      pars assoc p assoc' p' = if p > p' || (p == p' && assoc /= assoc') then noncap else id
      
      go _ _     One = ""
      go _ _     Dot = "."
      go _ _     (Chr a) = [a]
      go _ _     (Group False e) = noncap $ go OpAssocNone 0 e
      go _ _     (Group True e) = cap $ go OpAssocNone 0 e
      go assoc p (Concat e1 e2) = pars assoc p OpAssocRight 2
                                    (go OpAssocLeft 2 e1 ++ go OpAssocRight 2 e2)
      go assoc p (Branch e1 e2) = pars assoc p OpAssocRight 1
                                    (go OpAssocLeft 1 e1 ++ "|" ++ go OpAssocRight 1 e2)
      go _ _     (Class b rs) = "[" ++ (if b then "" else "^") ++ concatMap rng rs ++ "]"
      go _ _     (NamedSet b ns) = "[:" ++ (if b then "" else "^") ++ name ns ++ ":]"
      go _ _     (Range e i mj) = go OpAssocNone 3 e
                                  ++ "{" ++ show i
                                  ++ (case mj of
                                        Just j -> ","++show j
                                        Nothing -> "")
                                  ++ "}"
      go assoc p (LazyRange e i mj) =
        pars assoc p OpAssocNone 3 $
          go OpAssocNone 3 e
          ++ "{" ++ show i
          ++ (case mj of
                Just j -> ","++show j
                Nothing -> "")
          ++ "}?"
      go assoc p (Star e)     = pars assoc p OpAssocNone 3 $ go OpAssocNone 3 e ++ "*"
      go assoc p (LazyStar e) = pars assoc p OpAssocNone 3 $ go OpAssocNone 3 e ++ "*?"
      go assoc p (Plus e)     = pars assoc p OpAssocNone 3 $ go OpAssocNone 3 e ++ "+"
      go assoc p (LazyPlus e) = pars assoc p OpAssocNone 3 $ go OpAssocNone 3 e ++ "+?"
      go assoc p (Question e) = pars assoc p OpAssocNone 4 $ go OpAssocNone 4 e ++ "?"
      go assoc p (LazyQuestion e) = pars assoc p OpAssocNone 3 $ go OpAssocNone 3 e ++ "??"
      go _ _ (Suppress _) = error "No syntax for suppress"

      name ns = case ns of
                 NSAlnum  -> "alnum"
                 NSAlpha  -> "alpha"
                 NSAscii  -> "ascii"
                 NSBlank  -> "blank"
                 NSCntrl  -> "cntrl"
                 NSDigit  -> "digit"
                 NSGraph  -> "graph"
                 NSLower  -> "lower"
                 NSPrint  -> "print"
                 NSPunct  -> "punct"
                 NSSpace  -> "space"
                 NSUpper  -> "upper"
                 NSWord   -> "word"
                 NSXDigit -> "xdigit"

      rng (a,b) = [a,'-',b]

simplifyRanges :: [(Char, Char)] -> [(Char, Char)]
simplifyRanges rs = simplifyRanges' $ sort $ filter validRange rs
  where
    validRange (c1, c2) = c1 <= c2

    simplifyRanges' [] = []
    simplifyRanges' [r] = [r]
    simplifyRanges' ((c1, c2):(c1', c2'):rs')
        | c2' <= c2 = simplifyRanges' $ (c1, c2):rs'
        | c1' <= c2 || succ c2 == c1' = simplifyRanges' $ (c1, c2'):rs'
        | otherwise = (c1, c2):simplifyRanges' ((c1', c2'):rs')

-- | Negate character ranges. Precondition: Input ranges are simplified
-- (i.e. sorted and minimal)
negateRanges :: [(Char, Char)] -> [(Char, Char)]
negateRanges = negateRanges' $ Just $ chr 0
  where
    negateRanges' (Just c) [] = [(c, chr 255)]
    negateRanges' (Just c) ((c1, c2):rs) =
        let rs' = negateRanges' (if ord c2 < 255 then Just $ succ c2 else Nothing) rs
        in if c < c1 then (c, chr $ ord c1 - 1):rs' else rs'
    negateRanges' Nothing _ = []

balance :: [Int.Regex] -> Maybe Int.Regex
balance [] = Nothing
balance [c] = Just c
balance xs = let (xs1, xs2) = splitAt (length xs `div` 2) xs
              in (Int.:|:) <$> balance xs1 <*> balance xs2

reSum :: Maybe Int.Regex -> Maybe Int.Regex -> Maybe Int.Regex
reSum Nothing Nothing = Nothing
reSum Nothing (Just e) = Just e
reSum (Just e) Nothing = Just e
reSum (Just e1) (Just e2) = Just $ e1 Int.:|: e2

-- | Flag indicating how a ? is rewritten to a choice
data QuestionOrientation = EmptyLeft  -- ^ "E?" => "1 + E"
                         | EmptyRight -- ^ "E?" => "E + 1"
                         deriving (Show)

-- | How to treat the dot when simplifying?
data DotBehavior = BalancedTree -- ^ Build a balanced tree of alternatives
                 | DummyDot     -- ^ Insert some dummy char.  Useful
                                --   when interfacing with external tools.
                 deriving (Show)

-- | Convert regular expression AST to internal (simplified) representation
simplify' :: Int.GroupId -> QuestionOrientation -> DotBehavior -> Regex -> (Int.GroupId, Maybe Int.Regex)
simplify' i _ _ One = (i, Just Int.One)
simplify' i _ BalancedTree Dot = (i, balance [Int.Byte j | j <- [0..255]])
simplify' i o b@DummyDot Dot = simplify' i o b (Chr '.')
simplify' i _ _ (Chr c) = let b = fromIntegral . ord $ c in (i, Just $ Int.Byte b)
simplify' i o b (Group True re) = (Int.G i <$>) `second` simplify' (i+1) o b re
simplify' i o b (Group False re) = simplify' i o b re
simplify' i o b (Concat e1 e2) =
  let (i', r') = simplify' i o b e1
  in ((Int.:&:) <$> r' <*>) `second` simplify' i' o b e2
simplify' i o b (Branch e1 e2) =
  let (i', r') = simplify' i o b e1
  in reSum r' `second` simplify' i' o b e2
simplify' i o@EmptyRight b (Question e) =
  flip reSum (Just Int.One) `second` simplify' i o b e
simplify' i o@EmptyLeft b (Question e) =
  reSum (Just Int.One) `second` simplify' i o b e
simplify' i o b (Star e) = (Int.K <$>) `second` simplify' i o b e
simplify' i o b (Plus e) =
  ((\e' -> e' Int.:&: Int.K e') <$>) `second` simplify' i o b e
simplify' i _ _ (Class b rs) =
    let rs' = (if b then id else negateRanges) $ simplifyRanges rs
    in (,) i $ balance
            $ concat
                  [map Int.Byte [fromIntegral (ord c1)..fromIntegral (ord c2)]
                       | (c1, c2) <- rs']
simplify' _ _ _ (NamedSet _ n) = error $ "I can't simplify " ++ show n
simplify' i _ _ (Range _ from (Just to))
    | to < from = (i, Nothing)
simplify' i o b (Range e from mto)
    | from == 0, isNothing mto || mto == Just 0 = (i, Nothing)
    | otherwise = foldr1 combine $ map (simplify' i o b)
                                       (unroll from (fromMaybe from mto))
  where
    -- Group-ids under repetition are identical:
    --    (a){2} -> (:1 a)(:1 a), NOT (:1 a)(:2 a)
    combine (_,r') (i', acc) = (i', (Int.:&:) <$> r' <*> acc)
    unroll :: Int -> Int -> [Regex]
    unroll 0 1 = [Question e]
    unroll 0 y = Question e : unroll 0 (y-1)
    unroll 1 1 = [e]
    unroll x y = e : unroll (x-1) (y-1)

simplifyOpts :: QuestionOrientation -> DotBehavior -> Regex -> Maybe Int.Regex
simplifyOpts ori beh = snd . simplify' 1 ori beh

-- | Convert regular expression AST to internal (simplified) representation
simplify :: Regex -> Maybe Int.Regex
simplify = simplifyOpts EmptyRight BalancedTree
