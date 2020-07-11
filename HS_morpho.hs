module HS_morpho
    ( Labeled(Labeled)
    , Feature
    , feature
    , Morpheme
    , Stem
    , stem
    , Exponent
    , exponent
    , Category(N,A,V,D)
    , Array
    , array
    , Workspace(Workspace)
    , mkWorkspace
    , GEN
    , mkGen
    , Ranking
    , Constraint
    , Markedness
    , Faithfulness
    , mc
    , idF
    , mkMax
    , getMorphemes
    , converge
    ) where


-- import Fst
import Prelude hiding (exponent)
import Data.List (inits,tails,delete,nub)
import Control.Monad (guard)
import Data.Function ((&))
import Data.Either (rights)
import Data.Bifunctor (Bifunctor(first))
-- import Control.Lens

{-
Each stem (V,N,A,D...) has an associated set of features.
Items are taken from the Lexicon with their (fully specified) set of features.
No features are added.

Syntax gets the inflected word without structure but with all its features.
-}

{- Subject Principle
Compatability: (Ident)
The features of the exponent are a subset of the features of the stem
Specificity: (Max)
The Exponent is the most specic item that satisfies compatability
-}

data Labeled x = 
    Labeled { label :: String
            , content :: x} 
    deriving (Eq,Show)

type Feature = Labeled Bool
feature :: String -> Bool -> Feature
feature = Labeled

type Exponent = Labeled [Feature]
exponent :: String -> [Feature] -> Exponent
exponent = Labeled

type Array = Labeled [Exponent]
array :: String -> [Exponent] -> Array
array = Labeled

type Stem = Labeled Category
stem :: String -> Category -> Stem
stem = Labeled

type Morpheme = Either Stem Exponent
data Category = N | V | A | D deriving (Eq,Show)

data Workspace = 
    Workspace [Feature] [Array] [Morpheme] 
    deriving (Eq,Show)
type Grammar = (GEN,Ranking)

{-
Constraints could be DFSTs that target one of the three lists in the Workspace.
Except many seem to compare between features on Morphemes and features on the stem (this could mean faithfulness)
mc only checks the number of arrays (this could be markedness, unless we change it to check between IO)
-}

-- General helper-functions
mkWorkspace :: Either Stem Exponent -> [Feature] -> [Array] -> Workspace
mkWorkspace stem features arrays = 
    Workspace features arrays [stem]

split :: [a] -> [([a], [a])]
split xs = zip (inits xs) (tails xs)

getMorphemes :: Workspace -> String
getMorphemes (Workspace _ _ xs) =
    xs >>= either label label -- there has to be a less redundant way.

-- GEN:
type GEN = Workspace -> [Workspace]

mkGen :: Bool -> Bool -> GEN
mkGen allowMerge allowMove workspace
    | allowMerge = wrapMerge workspace 
      ++ mkGen False allowMove workspace
    | allowMove = wrapMove workspace
      ++ mkGen False False workspace
    | otherwise = pure workspace

wrapMerge :: Workspace -> [Workspace]
wrapMerge (Workspace features arrays morphemes) =
    merge (arrays,morphemes)
    & map (uncurry (Workspace features))

merge :: ([Array],[Morpheme]) -> [([Array],[Morpheme])]
merge (arrays,morphemes) =
    do
        pick <- arrays
        symbol <- content pick
        let (newArrays,symbol') = (delete pick arrays, symbol)
        (morph,emes) <- split morphemes
        return (newArrays,morph ++ (Right symbol' : emes))

wrapMove :: Workspace -> [Workspace]
wrapMove (Workspace features arrays morphemes) =
    map (Workspace features arrays) $ move morphemes

move :: Eq a => [a] -> [[a]]
move morphemes =
    do
        (morph,emes) <- split morphemes 
        guard (not $ null emes)
        let (morphemes',movee) = (morph ++ tail emes,head emes) 
        ((morph,emes),movee) <- map (\x -> (x,movee)) $ split morphemes'
        let morphemes' = morph ++ (movee : emes)
        guard (morphemes' /= morphemes)
        return morphemes'

-- Constraints:
type Ranking = [Constraint] -- change to allow (reflexive) conjunctions
type Constraint = Markedness -- so far all the features seem to be broadly markedness. If this doesn't change, delete features
type Markedness = Workspace -> [()]
type Faithfulness = Workspace -> Workspace -> [()]

-- reflexive conjunction is just counting in disguise. 
-- to call it a conjunction seems weird.
mkReflexive :: Constraint -> Int -> Constraint
mkReflexive con n workspace 
    | n <= length (con workspace) = [()]
    | otherwise = []                  

mkConjunction :: Constraint -> Constraint -> Constraint
mkConjunction con1 con2 workspace 
    | null (con1 workspace) || null (con2 workspace) = []
    | otherwise = [()]

mc :: Markedness -- maybe this should be (anti-)Faithfulness instead?
mc (Workspace _ arrays _) = () <$ arrays

mkMc :: String -> Workspace -> [()]
mkMc name (Workspace _ arrays _) = () <$ filter (== name) (map label arrays) 

idF :: Markedness
idF (Workspace features1 _ morphemes) =
    do 
        feature1 <- features1
        feature2 <- rights morphemes >>= content
        let (f1,f2) = (feature1,feature2)
        guard (label f1 == label f2)
        guard (content f1 /= content f2)

mkMax :: String -> Markedness
mkMax feature (Workspace features _ morphemes) =
    rights morphemes
    & max feature features
  where
    max f fs ms 
        | f `elem` map label fs 
          && f `notElem`
          (map content ms >>= map label) = [()]
        | otherwise = []

mkL :: String -> Markedness
mkL name (Workspace features arrays morphemes)
    | name `notElem` map label arrays = []
    | otherwise = () <$ takeWhile (/= name) (map label arrays)

mkR :: String -> Markedness
mkR array (Workspace features arrays morphemes)
    | array `notElem` map label arrays = []
    | otherwise = () <$ dropWhile (/= array) (map label arrays)

{- COHERENCE demands indexation of exponents
In the book it looks like Müller indexes the set of features a stem carries.
Which Morpheme is the stem that carries the features in Wardaman?
Is this even the right way to think about this?
Stems also need to be indexed, this could leed to another "labeled" situation...
Maybe the structure building features need to be indexed as well?
-}

-- EVAL
eval :: Grammar -> Workspace -> Workspace
eval (gen,ranking) workspace = 
    best ranking $ gen workspace 
  where 
    best []     candidates = head candidates
    best _     [candidate] = candidate
    best (c:cs) candidates = best cs $ optimals c candidates []
    optimals c []    acc = acc
    optimals c (x:xs) [] = optimals c xs [x]
    optimals c (x:xs) (a:cc) 
        | c x <  c a = optimals c xs [x] 
        | c x == c a = optimals c xs (x:a:cc)
        | c x >  c a = optimals c xs (a:cc)

cycles :: Grammar -> Workspace -> [Workspace]
cycles grammar = iterate (eval grammar)

converge :: Grammar -> Workspace -> Workspace
converge grammar input = dupl $ cycles grammar input
  where
    dupl (x:y:ys) 
        | x == y    = x
        | otherwise = dupl (y:ys)  
 
converge' :: Grammar -> Workspace -> [Workspace]
converge' grammar input = dupl $ cycles grammar input
  where 
    dupl (x:y:ys) 
        | x == y    = [x]
        | otherwise = x : dupl (y:ys)