module HS_morpho
    ( Feature(Binary)
    , Morpheme
    , Stem(Stem)
    , Exponent(Exponent)
    , Category(N,A,V,D)
    , Array(Array)
    , Workspace(Workspace)
    , mkWorkspace
    , GEN
    , mkGen
    , Ranking
    , Constraint
    , Markedness
    , Faithfulness
    , merge -- remove this
    , mc
    , idF
    , mkMax
    , getMorphemes
    , converge
    ) where


-- import Fst
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


-- most of the instances of Show are just for testing
data Feature = 
    Binary { featureName :: String
           , featureValue :: Bool} 
    deriving (Eq, Show)
-- add valued Features
data Category = N | V | A | D deriving (Eq,Show)-- Categories could just be Strings?
type Morpheme = Either Stem Exponent

data Exponent = 
    Exponent { exName :: String -- exname is a bad name
             , exFeatures :: [Feature]} 
    deriving (Eq,Show)

data Array = 
    Array { arrayName :: String
          , arrayContents :: [Exponent]
          } deriving (Eq,Show) -- maybe there should be typeclasses for getting names and contents

data Stem = 
    Stem { stName :: String 
         , stCategory :: Category} 
    deriving (Eq,Show) -- unsure if Category is needed. <- it is needed because it shoudl determine the set of features.

data Workspace = Workspace [Feature] [Array] [Morpheme] 
    deriving (Eq,Show)

type Grammar = (GEN,Ranking)

{-
Constraints could be DFSTs that target one of the three lists in the Workspace.
Except many seem to compare between features on Morphemes and features on the stem (this could mean faithfulness)
mc only checks the number of arrays (this could be markedness, unless we change it to check between IO)
-}

-- General helper-functions
mkWorkspace :: Either Stem Exponent -> [Feature] -> [Array] -> Workspace
mkWorkspace stem@(Left (Stem string category)) features arrays = 
    Workspace features arrays [stem]

split :: [a] -> [([a], [a])]
split xs = zip (inits xs) (tails xs)

getMorphemes :: Workspace -> String
getMorphemes (Workspace _ _ xs) =
    xs >>= either stName exName

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
        symbol <- arrayContents pick
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
type Ranking = [Constraint]
type Constraint = Markedness -- so far all the features seem to be broadly markedness. If this doesn't change, delete features
type Markedness = Workspace -> [()]
type Faithfulness = Workspace -> Workspace -> [()]

mc :: Markedness -- maybe this should be (anti-)Faithfulness instead?
mc (Workspace _ arrays _) = () <$ arrays

idF :: Markedness
idF (Workspace features1 _ morphemes) =
    do 
        feature1 <- features1
        feature2 <- rights morphemes >>= exFeatures
        let (f1,f2) = (feature1,feature2)
        guard (featureName f1 == featureName f2)
        guard (featureValue f1 /= featureValue f2)

mkMax :: String -> Markedness
mkMax feature (Workspace features _ morphemes) =
    rights morphemes
    & max feature features
  where
    max f fs ms 
        | f `elem` map featureName fs 
          && f `notElem`
          (map exFeatures ms >>= map featureName) = [()]
        | otherwise = []

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