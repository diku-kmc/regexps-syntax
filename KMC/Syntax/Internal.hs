module KMC.Syntax.Internal
    ( GroupId
    , Regex(..)
    , Value(..)
    , PDigit(..)
    , Pos
    , flat
    , clen
    , reify
    , showPos
    , vnorm
    , vpos
    , rsub
    , prettyPrint
    ) where

import           Control.Applicative ((<$>))
import           Data.Char           (chr, ord)
import           Data.List           (intercalate, sort)
import           Data.Word           (Word8)

modName :: String
modName = "KMC.Syntax.Internal"

type GroupId = Int

-- | Internal (simplified) datatype for regular expressions.
data Regex = Byte Word8       -- ^ Singleton byte
           | One              -- ^ Matches the empty word.
           | Regex :|: Regex  -- ^ Alternative/branching
           | Regex :&: Regex  -- ^ Concatenation
           | K Regex          -- ^ Kleene star
           | G GroupId Regex  -- ^ Group w./ an id
  deriving (Show, Eq, Ord)
  
-- | Very simple pretty printing. Does not try to adhere to any specific
-- external syntax, and will print excessive parentheses.
prettyPrint :: Regex -> String
prettyPrint (Byte w) | 0x20 <= w && w <= 0x7e = [chr $ fromIntegral w]
                     | otherwise = error $ "Character out of printable range: " ++ show w
prettyPrint One = "()"
prettyPrint (e1 :|: e2) = "(" ++ prettyPrint e1 ++ "|" ++ prettyPrint e2 ++ ")"
prettyPrint (e1 :&: e2) = "(" ++ prettyPrint e1 ++ prettyPrint e2 ++ ")"
prettyPrint (K e) = prettyPrint e ++ "*"
prettyPrint (G _ e) = prettyPrint e

infixl 9 :&:
infixl 8 :|:

data Value = Unit             -- ^ Unit value
           | Lit Word8        -- ^ Character literal
           | Inl Value        -- ^ Left alternative
           | Inr Value        -- ^ Right alternative
           | Pair Value Value -- ^ Concatenation
           | Fold Value       -- ^ List fold
           | Void             -- ^ Untypable term. Used only as a result of
                              -- projections when discarding irrelevant
                              -- alternatives.
  deriving (Eq, Ord)

instance Show (Value) where
    show Unit = "()"
    show (Lit w) = show w
    show (Inl v) = "L:" ++ show v
    show (Inr v) = "R:" ++ show v
    show (Pair v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
    show l@(Fold _) = "[" ++ (intercalate "," $ showL l) ++ "]"
        where showL (Fold (Inr Unit)) = []
              showL (Fold (Inl (Pair v v'))) = show v:showL v'
              showL _ = error $ "showList: Not a list"
    show Void = "âˆ…"

-- | Type checking
check :: Value -> Regex -> Bool
check Unit One                 = True
check (Lit w) (Byte w')        = w == w'
check (Pair v1 v2) (e1 :&: e2) = check v1 e1 && check v2 e2
check (Inl v1)     (e1 :|: _)  = check v1 e1
check (Inr v2)     (_ :|: e2)  = check v2 e2
check (Fold v)     (K e)       = check v ((e :&: (K e)) :|: One)
check _            _           = False

-- | Nullability check
nullable :: Regex -> Bool
nullable One = True
nullable (G _ e) = nullable e
nullable (Byte _) = False
nullable (K _) = True
nullable (e1 :|: e2) = nullable e1 || nullable e2
nullable (e1 :&: e2) = nullable e1 && nullable e2

-- | Calculate number of characters parsed by a given value
clen :: Value -> Int
clen Unit = 0
clen (Lit _) = 1
clen (Inl v) = clen v
clen (Inr v) = clen v
clen (Fold v) = clen v
clen (Pair v1 v2) = clen v1 + clen v2
clen Void = error "Void value"

-- | Flattening function
flat :: Value -> [Word8]
flat Unit = []
flat (Lit w) = [w]
flat (Inl v) = flat v
flat (Inr v) = flat v
flat (Fold v) = flat v
flat (Pair v1 v2) = flat v1 ++ flat v2
flat Void = [] -- technically an error, but we would like to be able to flatten projections

-- | Position digit, as in Okui/Suzuki. All our nodes are at most binary, so a
-- finite domain is sufficient for now.
data PDigit = P1 | P2
  deriving (Show, Eq, Ord)
-- | Positions, as in Okui/Suzuki. A position is a (possibly empty) sequence of
-- digits uniquely determining a node in a syntax tree. We will use positions in
-- the context of both regular expressions and values.
type Pos = [PDigit]

showPos :: Pos -> String
showPos [] = "Îµ"
showPos xs = map f xs
    where f p = case p of
                  P1 -> '1'
                  P2 -> '2'

-- | Compute the set of all positions for a given value.
vpos :: Value -> [Pos]
vpos Unit = [[]]
vpos (Lit _) = [[]]
vpos Void = [[]]
vpos (Inl v) = []:(map (P1:) $ vpos v)
vpos (Inr v) = []:(map (P2:) $ vpos v)
vpos (Pair v1 v2) = []:((map (P1:) $ vpos v1) ++ (map (P2:) $ vpos v2))
vpos (Fold v) = []:(map (P1:) $ vpos v)

-- | Compute the set of all positions for a given regex.
rpos :: Regex -> [Pos]
rpos (Byte _) = [[]]
rpos One = [[]]
rpos (r1 :|: r2) = []:((map (P1:) $ rpos r1) ++ (map (P2:) $ rpos r2))
rpos (r1 :&: r2) = []:((map (P1:) $ rpos r1) ++ (map (P2:) $ rpos r2))
rpos (K r) = []:(map (P1:) $ rpos r)
rpos (G _ r) = rpos r

-- | Extract (possibly non-existing) subtree for regex at given position
rsub :: Regex -> Pos -> Maybe Regex
rsub (G _ r) p = rsub r p
rsub (e1 :|: _) (P1:p) = rsub e1 p
rsub (_ :|: e2) (P2:p) = rsub e2 p
rsub (e1 :&: _) (P1:p) = rsub e1 p
rsub (_ :&: e2) (P2:p) = rsub e2 p
rsub (K e) (P1:p) = rsub e p
rsub (K _) (P2:_) = Nothing
rsub e [] = Just e
rsub (Byte _) (_:_) = Nothing
rsub One (_:_) = Nothing

-- | Extract (possibly non-existing) subtree for value at given position
vsub :: Value -> Pos -> Maybe Value
vsub v [] = Just v
vsub (Inl v) (P1:p) = vsub v p
vsub (Inr v) (P2:p) = vsub v p
vsub (Pair v1 _) (P1:p) = vsub v1 p
vsub (Pair _  v2) (P2:p) = vsub v2 p
vsub (Fold v) (P1:p) = vsub v p
vsub _ _ = Nothing

-- | Compute the norm of a value at a given position
vnorm :: Value -> Pos -> Int
vnorm v p = maybe (-1) clen (vsub v p)

-- | A Binder is a path through a regular expression which uniquely determines
-- the position of a given group.
data Binder = BHere | BLeft Binder | BRight Binder

instance Show Binder where
  show BHere      = "."
  show (BLeft b)  = '<':show b
  show (BRight b) = '>':show b

-- | Calculate the number of groups in a given regular expression
groupCount :: Regex -> Int
groupCount (e1 :&: e2) = groupCount e1 + groupCount e2
groupCount (e1 :|: e2) = groupCount e1 + groupCount e2
groupCount (K e1) = groupCount e1
groupCount (G _ e) = 1 + groupCount e
groupCount (Byte _) = 0
groupCount One = 0

-- | Extract the binder for a given group index. Groups are indexed as per the
-- POSIX standard, i.e., the i'th group is defined by the i'th opening
-- parenthesis in the textual representation of a regular expression, counting
-- from the left. The 0'th group implicitly represents the full regular
-- expression.
binder :: Regex -> Int -> Maybe Binder
binder re gi = case binder' re gi of
                Right b -> Just b
                Left _ -> Nothing
    where
      binder' _ 0 = Right BHere
      binder' (G _ e) i = binder' e (i-1)
      binder' (e1 :&: e2) i =
          case binder' e1 i of
            Right b' -> Right (BLeft b')
            Left i' -> BRight <$> binder' e2 i'
      binder' (e1 :|: e2) i =
          case binder' e1 i of
            Right b' -> Right (BLeft b')
            Left i' -> BRight <$> binder' e2 i'
      binder' (K e) i = binder' e i
      binder' _ i = Left i

-- | Injection/reification from Haskell types to parse trees
class Inject a where
  -- | Encode a Haskell value as a parse tree
  inj :: a -> Value
  -- | Decode a parse tree into a Haskell value
  reify :: Value -> a

instance (Inject a) => Inject [a] where
    inj [] = Fold (Inr Unit)
    inj (x:xs) = Fold (Inl (Pair (inj x) (inj xs)))
    reify (Fold (Inr Unit)) = []
    reify (Fold (Inl (Pair x xs))) = reify x:reify xs
    reify v = error $ "Malformed []-value " ++ show v

instance (Inject a, Inject b) => Inject (Either a b) where
    inj (Left x) = Inl $ inj x
    inj (Right x) = Inr $ inj x
    reify (Inl x) = Left $ reify x
    reify (Inr x) = Right $ reify x
    reify v = error $ "Malformed Either- value: " ++ show v

instance (Inject a, Inject b) => Inject (a, b) where
    inj (x, y) = Pair (inj x) (inj y)
    reify (Pair x y) = (reify x, reify y)
    reify v = error $ "Malformed (,)-value: " ++ show v

instance Inject () where
    inj () = Unit
    reify Unit = ()
    reify v = error $ "Malformed ()-value: " ++ show v

instance Inject Char where
    inj c = Lit $ fromIntegral $ ord $ c
    reify (Lit w8) = chr $ fromIntegral w8
    reify v = error $ "Malformed Char-value: " ++ show v

instance Inject Value where
    inj v = v
    reify v = v

-- | String wrapper type, to provide alternative interpretation of Haskell
-- strings as left-associative concatenations of chars. E.g.
--
-- >>> reify $ inj $ StrL "abc" :: ((Char, Char), Char)
-- (('a','b'),'c')
newtype StrL = StrL String
    deriving (Show)

instance Inject StrL where
    inj (StrL str) = inj' $ reverse str
        where inj' [] = error "empty string"
              inj' [x] = inj x
              inj' (x:xs) = Pair (inj' xs) (inj x)
    reify (Lit w8) = StrL [chr $ fromIntegral w8]
    reify (Pair xs (Lit w8)) = let StrL xs' = reify xs
                                in StrL $ xs' ++ [chr $ fromIntegral w8]
    reify v = error $ "Malformed StrL-value: " ++ show v

instance Inject a => Inject (Maybe a) where
    inj Nothing = Void
    inj (Just x) = inj x
    reify Void = Nothing
    reify x = Just $ reify x

-- | Project a value/parse tree against a given binder. This removes all
-- information that is not contained within the group identified by the
-- binder. Projection preserves list structure to prevent ambiguity when dealing
-- with groups under Kleene stars. E.g., we have
--
-- >>> let v = inj ([Left ('a', 'b'), Right 'x', Left ('b','a')] :: [Either (Char, Char) Char])
-- >>>  in reify $ project v (BLeft (BLeft BHere)) :: [Maybe Char]
-- [Just 'a',Nothing,Just 'b']
project :: Value -> Binder -> Value
project v BHere = v
project (Inl _) (BRight _) = Void
project (Inr _) (BLeft _) = Void
project (Inl v) (BLeft b) = project v b
project (Inr v) (BRight b) = project v b
project (Pair v1 _) (BLeft b) = project v1 b
project (Pair _  v2) (BRight b) = project v2 b
project v@(Fold (Inr Unit)) _ = v
project (Fold (Inl (Pair v vs))) b =
  Fold $ Inl $ Pair (project v b) (project vs b)
project _ _ = Void
