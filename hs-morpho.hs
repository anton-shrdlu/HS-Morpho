import Fst
import Data.List (inits,tails,delete,nub)
import Control.Monad (guard)
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


data Feature = Binary String Bool deriving (Eq, Show)
-- add valued Features
data Category = N | V | A -- Categories could just be Strings?

type Morpheme  = Either Stem Exponent
data Exponent  = Exponent String [Feature]
data Array     = Array [Exponent]
data Stem      = Stem Category String -- unsure if Category is needed.
data Workspace = Workspace [Feature] [Array] [Morpheme]
{-
Constraints could be DFSTs that target one of the three lists in the Workspace.
-}
mkWorkspace :: Either Stem Exponent -> [Feature] -> [Array] -> Workspace
mkWorkspace stem@(Left (Stem category string)) features arrays = 
    Workspace features arrays [stem]

split :: [a] -> [([a], [a])]
split xs = zip (inits xs) (tails xs)

merge :: Eq a => ([[a]], [a]) -> [([[a]], [a])]
merge (arrays,workspace) = 
    do
        pick <- arrays
        let taggedArray = (delete pick arrays,pick)
        symbol <- pick
        let (array,symbol') = symbol <$ taggedArray
        (work,space) <- split workspace
        return (array, work ++ (symbol' : space)) 

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
