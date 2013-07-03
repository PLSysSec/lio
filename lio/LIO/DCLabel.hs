{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

/Disjunction Category Labels/ ('DCLabel's) are a label format that
encodes authority, secrecy restrictions, and integrity properties
using propositional logic.

A 'DCLabel' consists of two boolean formulas over 'Principal's.  Each
formula is in conjunctive normal form, represented by type 'CNF'.  The
first 'CNF' ('dcSecrecy') specifies what combinations of principals
are allowed to make data public.  The second 'CNF' ('dcIntegrity')
specifies which combinations of principals have endorsed the integrity
of the data.

The '%%' operator allows one to construct a 'DCLabel' by joining a
secrecy 'CNF' on the left with an integrity 'CNF' on the right.  A
'DCLabel' can also be directly constructed with the constructor
'DCLabel'.  However, '%%' has the added convenience of accepting any
argument types that are instances of 'ToCNF'.

For example, the following expresses data that can be exported by the
principal \"Alice\" and may have been written by anybody:  @\"Alice\"
'%%' 'True'@.  (@'toCNF' 'True'@ indicates a trivially satisfiable
label component, which in this case means a label conveying no
integrity properties.)

A 'CNF' is created using the ('\/') and ('/\') operators.  The
disjunction operator ('\/') is used to compute a 'CNF' equivalent to
the disjunciton of two 'Principal's, 'Strings', or 'CNF's. For
example:

  @
p1 = 'principal' \"p1\"
p2 = 'principal' \"p2\"
p3 = 'principal' \"p3\"
e1 = p1 '\/' p2
e2 = e1 '\/' \"p4\"
  @

Similarly, the conjunction operator ('/\') creates a 'CNF' as a
conjunction of 'Principal's, 'String's, 'Disjunction's, or 'CNF's.

  @
e3 = p1 '\/' p2
e4 = e1 '/\' \"p4\" '/\' p3
  @

Note that because a 'CNF' formula is stored as a conjunction of
'Disjunction's, it is much more efficient to apply '/\' to the result
of '\/' than vice versa.  It would be logical for '/\' to have higher
fixity than '\/'.  Unfortunately, this makes formulas harder to read
(given the convention of AND binding more tightly than OR).  Hence
'\/' and '/\' have been given the same fixity but different
associativities, preventing the two from being mixed in the same
expression without explicit parentheses.

Consider the following, example:

  @
cnf1 = (\"Alice\" '\/' \"Bob\") '/\' \"Carla\"
cnf2 = \"Alice\" '/\' \"Carla\"
dc1 = cnf1 '%%' cnf2
dc2 = \"Djon\" '%%' \"Alice\"
pr = PrivTCB $ \"Alice\" '/\' \"Carla\"
  @

This will result in the following:

>>> dc1
"Carla" /\ ("Alice" \/ "Bob") %% "Alice" /\ "Carla"
>>> dc2
"Djon" %% "Alice"
>>> canFlowTo dc1 dc2
False
>>> canFlowToP pr dc1 dc2
True

Because the '\/' and '/\' operators accept strings and 'Principal's as
well as 'CNF's, it is sometimes easy to forget that strings and
'Principal's are not actually 'CNF's.  For example:

>>> "Alice" /\ "Bob" `speaksFor` "Alice" \/ "Bob"
True
>>> "Alice" `speaksFor` "Alice" \/ "Bob"
<interactive>:12:21:
    Couldn't match expected type `[Char]' with actual type `CNF'

To convert a single string or 'Principal' to a 'CNF', you must use the
'toCNF' method:

>>> toCNF "Alice" `speaksFor` "Alice" \/ "Bob"
True

-}

module LIO.DCLabel (
  -- * Top-level aliases and functions
    DC, DCPriv, DCLabeled, dcDefaultState, evalDC, tryDC
  -- * Main types and functions
  , Principal, principalBS, principal
  , DCLabel(..), dcPublic, (%%), (/\), (\/)
  , CNF, ToCNF(..)
  -- * Lower-level functions
  , principalName
  , Disjunction, dToSet, dFromList
  , cTrue, cFalse, cToSet, cFromList
  ) where

import safe Control.Applicative
import safe Data.Bits
import safe qualified Data.ByteString as S
import Data.Hashable
import safe Data.List
import safe Data.Monoid
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.String
import safe Data.Typeable
import safe Data.Word
import safe Text.Read

import safe LIO.Exception (SomeException)
import safe LIO.Core
import safe LIO.Label
import safe LIO.Labeled
import safe LIO.Run

type SetTag = Word64

--
-- Principals
--

-- | A @Principal@ is a primitive source of authority, represented as
-- a string.  The interpretation of principal strings is up to the
-- application.  Reasonable schemes include encoding user names,
-- domain names, and/or URLs in the 'Principal' type.
data Principal = Principal !S.ByteString {-# UNPACK #-} !SetTag
                 deriving (Ord, Typeable)

instance Show Principal where
  showsPrec _ (Principal n _) = shows n

instance Read Principal where
  readsPrec d s = do
    (name, rest) <- readsPrec d s
    return (principalBS name, rest)

instance Eq Principal where
  (Principal n1 t1) == (Principal n2 t2) = t1 == t2 && n1 == n2

-- | Extract the name of a principal as a strict 'S.ByteString'.
-- (Use 'show' to get it as a regular 'String'.)
principalName :: Principal -> S.ByteString
{-# INLINE principalName #-}
principalName (Principal n _) = n

-- | Create a principal from a strict 'S.ByteString'.
principalBS :: S.ByteString -> Principal
principalBS bs = Principal bs bloom
  where hv = hash bs
        bloom = bit (hv .&. 0x3f)
                .|. (bit $ shiftR hv 6 .&. 0x3f)
                .|. (bit $ shiftR hv 12 .&. 0x3f)

-- | Create a principal from a 'String'.  The 'String' is packed into
-- a 'S.ByteString' using 'fromString', which will almost certainly
-- give unexpected results for non-ASCII unicode code points.
principal :: String -> Principal
principal = principalBS . fromString

--
-- Disjunctive clauses (a.k.a. "disjunction categories")
--

-- | Represents a disjunction of 'Principal's, or one clause of a
-- 'CNF'.  There is generally not much need to work directly with
-- @Disjunction@s unless you need to serialize and de-serialize them
-- (by means of 'dToSet' and 'dFromList').
data Disjunction = Disjunction !(Set Principal) {-# UNPACK #-} !SetTag
                   deriving (Typeable)

-- | Expose the set of 'Principal's being ORed together in a
-- 'Disjunction'.
dToSet :: Disjunction -> Set Principal
dToSet (Disjunction ps _) = ps

instance Eq Disjunction where
  (Disjunction ps1 t1) == (Disjunction ps2 t2) = t1 == t2 && ps1 == ps2

instance Ord Disjunction where
  compare (Disjunction ps1 _) (Disjunction ps2 _) =
    case compare (Set.size ps1) (Set.size ps2) of
      EQ -> compare ps1 ps2
      o  -> o

instance Show Disjunction where
  showsPrec _ (Disjunction ps _)
    | Set.size ps == 0 = ("False" ++)
    | Set.size ps == 1 = shows $ Set.findMin ps
    | otherwise = showParen True $
        foldr1 (\l r -> l . (" \\/ " ++) . r) $ map shows $ Set.toList ps

-- | Note that a disjunction containing more than one element /must/
-- be surrounded by parentheses to parse correctly.
instance Read Disjunction where
  readPrec = false <++ clause <++ single
    where false = do False <- readPrec; return dFalse
          single = dSingleton <$> readPrec
          clause = parens $ prec minPrec $ do
            let next = do Symbol "\\/" <- lexP
                          liftA2 (:) readPrec next
                       <++ pure []
            dFromList <$> liftA2 (:) readPrec next

instance Monoid Disjunction where
  mempty = dFalse
  mappend = dUnion

dFalse :: Disjunction
dFalse = Disjunction Set.empty 0

dSingleton :: Principal -> Disjunction
dSingleton p@(Principal _ t) = Disjunction (Set.singleton p) t

dUnion :: Disjunction -> Disjunction -> Disjunction
dUnion (Disjunction ps1 t1) (Disjunction ps2 t2) =
  Disjunction (Set.union ps1 ps2) (t1 .|. t2)

-- | Convert a list of 'Principal's into a 'Disjunction'.
dFromList :: [Principal] -> Disjunction
dFromList pl = Disjunction (Set.fromList pl) tres
  where tres = foldl' (\tl (Principal _ tr) -> tl .|. tr) 0 pl

-- | Returns 'True' iff the first disjunction is a subset of the second.
dImplies :: Disjunction -> Disjunction -> Bool
dImplies (Disjunction ps1 t1) (Disjunction ps2 t2)
  | t1 .&. t2 /= t1 = False
  | otherwise       = ps1 `Set.isSubsetOf` ps2

--
-- Conjunctive Normal Form (CNF) Formulas
--

-- | A boolean formula in Conjunctive Normal Form.  @CNF@ is used to
-- describe 'DCLabel' privileges, as well to provide each of the two
-- halves of a 'DCLabel'.
newtype CNF = CNF (Set Disjunction) deriving (Eq, Ord, Typeable)

-- | Convert a 'CNF' to a 'Set' of 'Disjunction's.  Mostly useful if
-- you wish to serialize a 'DCLabel'.
cToSet :: CNF -> Set Disjunction
{-# INLINE cToSet #-}
cToSet (CNF ds) = ds

instance Show CNF where
  showsPrec d (CNF ds)
    | Set.size ds == 0 = ("True" ++)
    | Set.size ds == 1 = shows $ Set.findMin ds
    | otherwise = showParen (d > 7) $
        foldr1 (\l r -> l . (" /\\ " ++) . r) $ map shows $ Set.toList ds

instance Read CNF where
  readPrec = true <++ formula <++ single
    where true = do True <- readPrec; return cTrue
          single = cSingleton <$> readPrec
          formula = parens $ prec 7 $ do
            let next = do Symbol "/\\" <- lexP
                          liftA2 (:) readPrec next
                       <++ pure []
            cFromList <$> liftA2 (:) readPrec next

instance Monoid CNF where
  mempty = cTrue
  mappend = cUnion
  
-- | A 'CNF' that is always @True@--i.e., trivially satisfiable.  When
-- @'dcSecrecy' = cTrue@, it means data is public.  When
-- @'dcIntegrity' = cTrue@, it means data carries no integrity
-- guarantees.  As a description of privileges, @cTrue@ conveys no
-- privileges; @'canFlowToP' cTrue l1 l2@ is equivalent to
-- @'canFlowTo' l1 l2@.
--
-- Note that @'toCNF' 'True' = cTrue@.  Hence @'dcPublic' = 'DCLabel'
-- cTrue cTrue@.
cTrue :: CNF
cTrue = CNF $ Set.empty

-- | A 'CNF' that is always @False@.  If @'dcSecrecy' = cFalse@, then
-- no combination of principals is powerful enough to make the data
-- public.  For that reason, @cFalse@ generally shouldn't appear in a
-- data label.  However, it is convenient to include as the
-- 'dcSecrecy' component of 'lioClearance' to indicate a thread may
-- arbitrarily raise its label.
--
-- @'dcIntegrity' = cFalse@ indicates impossibly much integrity--i.e.,
-- data that no combination of principals is powerful enough to modify
-- or have created.  Generally this is not a useful concept.
--
-- As a privilege description, @cFalse@ indicates impossibly high
-- privileges (i.e., higher than could be achieved through any
-- combination of 'Principal's).  @cFalse ``speaksFor`` p@ for any
-- 'CNF' @p@.  This can be a useful concept for bootstrapping
-- privileges within the 'DC' monad itself.  For instance, the result
-- of @'privInit' cFalse@ can be passed to fully-trusted 'DC' code,
-- which can in turn use 'delegate' to create arbitrary finite
-- privileges to pass to less privileged code.
cFalse :: CNF
cFalse = CNF $ Set.singleton dFalse

cSingleton :: Disjunction -> CNF
cSingleton = CNF . Set.singleton

setAny :: (a -> Bool) -> Set a -> Bool
setAny prd = Set.foldr' (\a -> (prd a ||)) False

setAll :: (a -> Bool) -> Set a -> Bool
setAll prd = Set.foldr' (\a -> (prd a &&)) True

cInsert :: Disjunction -> CNF -> CNF
cInsert dnew c@(CNF ds)
  | setAny (`dImplies` dnew) ds = c
  | otherwise = CNF $ Set.insert dnew $ Set.filter (not . (dnew `dImplies`)) ds

cUnion :: CNF -> CNF -> CNF
cUnion c (CNF ds) = Set.foldr cInsert c ds

cOr :: CNF -> CNF -> CNF
cOr (CNF ds1) (CNF ds2) =
  cFromList $ [dUnion d1 d2 | d1 <- Set.toList ds1, d2 <- Set.toList ds2]

-- | Convert a list of 'Disjunction's into a 'CNF'.  Mostly useful if
-- you wish to de-serialize a 'CNF'.
cFromList :: [Disjunction] -> CNF
cFromList = Set.foldr cInsert cTrue . Set.fromList

cImplies1 :: CNF -> Disjunction -> Bool
cImplies1 (CNF ds) d = setAny (`dImplies` d) ds

cImplies :: CNF -> CNF -> Bool
cImplies c (CNF ds) = setAll (c `cImplies1`) ds

--
-- DCLabel
--

-- | Main DCLabel type.  @DCLabel@s use 'CNF' boolean formulas over
-- principals to express authority exercised by a combination of
-- principals.  A @DCLabel@ contains two 'CNF's.  One, 'dcSecrecy',
-- specifies the minimum authority required to make data with the
-- label completely public.  The second, 'dcIntegrity', expresses the
-- minimum authority that was used to endorse data with the label, or,
-- for mutable objects, the minimum authority required to modify the
-- object.
--
-- @DCLabel@s are more conveniently expressed using the '%%' operator,
-- with 'dcSecrecy' on the left and 'dcIntegrity' on the right, i.e.:
-- @(@/dcSecrecyValue/ '%%' /dcIntegrityValue/@)@.
--
-- @DCLabel@s enforce the following relations:
--
--   * If @cnf1@ and @cnf2@ are 'CNF's describing authority, then
--   @cnf1 ``speaksFor`` cnf2@ if and only if @cnf1@ logically implies
--   @cnf2@ (often written @cnf1 &#x27f9; cnf2@).  For example,
--   @(\"A\" '/\' \"B\") ``speaksFor`` 'toCNF' \"A\"@, while @'toCNF'
--   \"A\" ``speaksFor`` (\"A\" '\/' \"C\")@.
--
--   * Given two @DCLabel@s @dc1 = (s1 '%%' i1)@ and @dc2 = (s2 '%%'
--   i2)@, @dc1 ``canFlowTo`` dc2@ (often written @dc1@ &#8849; @dc2@)
--   if and only if @s2 ``speaksFor`` s1 && i1 ``speaksFor`` i2@.  In
--   other words, data can flow in the direction of requiring more
--   authority to make it public or removing integrity endorsements.
--
--   * Given two @DCLabel@s @dc1 = (s1 '%%' i1)@ and @dc2 = (s2 '%%'
--   i2)@, and a @p::'CNF'@ representing privileges, @'canFlowToP' p
--   dc1 dc2@ (often written @dc1@ &#8849;&#8346; @dc2@) if and only
--   if @(p '/\' s2) ``speaksFor`` s2 && (p '/\' i1) ``speaksFor``
--   i2@.
data DCLabel = DCLabel { dcSecrecy :: !CNF
                         -- ^ Describes the authority required to make
                         -- the data public.
                       , dcIntegrity :: !CNF
                         -- ^ Describes the authority with which
                         -- immutable data was endorsed, or the
                         -- authority required to modify mutable data.
                       } deriving (Eq, Ord, Typeable)

instance Show DCLabel where
  showsPrec d (DCLabel sec int) =
    showParen (d > 5) $ shows sec . (" %% " ++) . shows int

instance Read DCLabel where
  readPrec = parens $ prec 5 $ do
    sec <- readPrec
    Symbol "%%" <- lexP
    int <- readPrec
    return $ DCLabel sec int

-- |
-- > dcPublic = True %% True
--
-- This label corresponds to public data with no integrity guarantees.
-- For instance, an unrestricted Internet socket should be labeled
-- @dcPublic@.  The significance of @dcPublic@ is that given data
-- labeled @(s %% i)@, @s@ is the exact minimum authority such that
-- @(s %% i) &#x2291;&#x209b; dcPublic@, while @i@ is the exact
-- minimum authority such that @dcPublic &#x2291;&#x1d62; (s %% i)@.
dcPublic :: DCLabel
dcPublic = True %% True

-- | As a type, a 'CNF' is always a conjunction of 'Disjunction's of
-- 'Principal's.  However, mathematically speaking, a single
-- 'Principal' or single 'Disjunction' is also a degenerate example of
-- conjunctive normal form.  Class 'ToCNF' abstracts over the
-- differences between these types, promoting them all to 'CNF'.
class ToCNF c where toCNF :: c -> CNF
instance ToCNF CNF where toCNF = id
instance ToCNF (Priv CNF) where toCNF = privDesc
instance ToCNF Disjunction where toCNF = cSingleton
instance ToCNF Principal where toCNF = toCNF . dSingleton
instance ToCNF [Char] where toCNF = toCNF . principal
instance ToCNF Bool where
  toCNF True = cTrue
  toCNF False = cFalse

-- | The primary way of creating a 'DCLabel'.  The secrecy component
-- goes on the left, while the integrity component goes on the right,
-- e.g.:
--
-- > label = secrecyCNF %% integrityCNF
--
-- Unlike the 'DCLabel' constructor, the arguments can be any instance
-- of 'ToCNF'.  @%%@ has fixity:
--
-- > infix 6 %%
(%%) :: (ToCNF a, ToCNF b) => a -> b -> DCLabel
a %% b = toCNF a `DCLabel` toCNF b
infix 6 %%

-- | Compute a conjunction of two 'CNF's or 'ToCNF' instances.
--
-- Has fixity:
--
-- > infixr 7 /\
(/\) :: (ToCNF a, ToCNF b) => a -> b -> CNF
a /\ b = toCNF a `cUnion` toCNF b
infixr 7 /\

-- | Compute a disjunction of two 'CNF's or 'ToCNF' instances.  Note
-- that this can be an expensive operation if the inputs have many
-- conjunctions.
--
-- The fixity is specifically chosen so that @&#92;&#47;@ and '/\'
-- cannot be mixed in the same expression without parentheses:
--
-- > infixl 7 \/
(\/) :: (ToCNF a, ToCNF b) => a -> b -> CNF
a \/ b = toCNF a `cOr` toCNF b
infixl 7 \/

instance Label DCLabel where
  lub (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cUnion s1 s2) (cOr i1 i2)
  glb (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cOr s1 s2) (cUnion i1 i2)
  canFlowTo (DCLabel s1 i1) (DCLabel s2 i2) = cImplies s2 s1 && cImplies i1 i2

instance SpeaksFor CNF where
  {-# INLINE speaksFor #-}
  speaksFor = cImplies

dcMaxDowngrade :: CNF -> DCLabel -> DCLabel
dcMaxDowngrade p (DCLabel (CNF ds) int) = DCLabel sec (cUnion p int)
  where sec = CNF $ Set.filter (not . cImplies1 p) ds

instance PrivDesc DCLabel CNF where
  downgradeP = dcMaxDowngrade
  canFlowToP p (DCLabel s1 i1) (DCLabel s2 i2) =
    cImplies (cUnion p s2) s1 && cImplies (cUnion p i1) i2

--
-- Type aliases
--

-- | A common default starting state, where @'lioLabel' = 'dcPublic'@
-- and @'lioClearance' = False '%%' True@ (i.e., the highest
-- possible clearance).
dcDefaultState :: LIOState DCLabel
dcDefaultState = LIOState { lioLabel = dcPublic
                          , lioClearance = False %% True }

-- | The main monad type alias to use for 'LIO' computations that are
-- specific to 'DCLabel's.
type DC = LIO DCLabel

-- | 'DCLabel' privileges are expressed as a 'CNF' of the principals
-- whose authority is being exercised.
type DCPriv = Priv CNF

-- | An alias for 'Labeled' values labeled with a 'DCLabel'.
type DCLabeled = Labeled DCLabel

-- | Wrapper function for running @'LIO' 'DCLabel'@ computations.
--
-- @
-- evalDC dc = 'evalLIO' dc 'dcDefaultState'
-- @
evalDC :: DC a -> IO a
evalDC dc = evalLIO dc dcDefaultState

-- | 'DCLabel' wrapper for 'tryLIO':
--
-- @
-- tryDC dc = 'tryLIO' dc 'dcDefaultState'
-- @
tryDC :: DC a -> IO (Either SomeException a, LIOState DCLabel)
tryDC dc = tryLIO dc dcDefaultState
