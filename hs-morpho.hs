module HS_morpho
    ( Feature(Binary) 
    , Category
    , Morpheme
    , Exponent(Exponent)
    , Array(Array)
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
Tems are taken from the Lexicon with their (fully specified) set of features.
No features are added.

Syntax gets the inflected word without structure but with all its features.
-}

{- Subject Principle
Compatability: (Ident)
The features of the exponent are a subset of the features of the stem
Specificity: (Max)
The Exponent is the most specic item that satisfies compatability
-}


data Feature = 
    Binary { featureName :: String
           , featureValue :: Bool} 
    deriving (Eq, Show)
-- add valued Features
data Category = N | V | A deriving Eq-- Categories could just be Strings?
type Morpheme = Either Stem Exponent

data Exponent = 
    Exponent { exName :: String
             , exFeatures :: [Feature]} 
    deriving Eq

data Array = Array {exponents :: [Exponent]} 
    deriving Eq

data Stem = Stem String Category 
    deriving Eq -- unsure if Category is needed.

data Workspace = Workspace [Feature] [Array] [Morpheme] 
    deriving Eq

{-
Constraints could be DFSTs that target one of the three lists in the Workspace.
-}

-- General helper-functions
mkWorkspace :: Either Stem Exponent -> [Feature] -> [Array] -> Workspace
mkWorkspace stem@(Left (Stem string category)) features arrays = 
    Workspace features arrays [stem]

-- GEN:
type GEN = Workspace -> [Workspace]

mkGen :: Bool -> Bool -> GEN
mkGen allowMerge allowMove workspace
    | allowMerge = wrapMerge workspace 
      ++ mkGen False allowMove workspace
    | allowMove = wrapMove workspace
      ++ mkGen False False workspace
    | otherwise = pure workspace

split :: [a] -> [([a], [a])]
split xs = zip (inits xs) (tails xs)

wrapMerge :: Workspace -> [Workspace]
wrapMerge (Workspace features arrays morphemes) =
    merge (map (map Right . exponents) arrays,morphemes)
    & map (first (map (Array . rights)))
    & map (uncurry (Workspace features))

merge :: Eq a => ([[a]], [a]) -> [([[a]], [a])]
merge (arrays,workspace) = 
    do
        pick <- arrays
        let taggedArray = (delete pick arrays,pick)
        symbol <- pick
        let (array,symbol') = symbol <$ taggedArray
        (work,space) <- split workspace
        return (array, work ++ (symbol' : space)) 

wrapMove :: Workspace -> [Workspace]
wrapMove (Workspace features arrays morphemes) =
    map (Workspace features arrays) $ move morphemes

move :: Eq a => [a] -> [[a]]
move workspace =
    do
        (work,space) <- split workspace 
        guard (not $ null space)
        let (workspace',movee) = (work ++ tail space,head space) 
        ((work,space),movee) <- map (\x -> (x,movee)) $ split workspace'
        let workspace' = work ++ (movee : space)
        guard (workspace' /= workspace)
        return workspace'

-- Constraints:
type Ranking = [Constraint]
type Constraint = Either Faithfulness Markedness
type Markedness = Workspace -> [()]
type Faithfulness = Workspace -> Workspace -> [()]

mc :: Markedness -- maybe this should be (anti-)Faithfulness instead?
mc (Workspace features arrays morphemes) = () <$ arrays

idF :: Faithfulness
idF (Workspace features1 _ _) (Workspace features2 _ _) =
    do 
        feature1 <- features1
        feature2 <- features2
        let (f1,f2) = (feature1,feature2)
        guard (featureName f1 == featureName f2)
        guard (featureValue f1 /= featureValue f2)

mkMax :: Feature -> Markedness -- maybe String -> Markedness is better
mkMax feature (Workspace features _ morphemes) =
    rights morphemes
    & max feature features
  where
    max f fs ms 
        | f `elem` fs 
          && notElem (featureName f) 
          (map exFeatures ms >>= map featureName) = [()]
        | otherwise = []
