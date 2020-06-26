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
data Category = N | V | A -- Maybe Category could just be String.

data Exponent  = Exponent String [Feature]
data Stem      = Stem Category String [Feature] -- unsure if Category is needed.
type Morpheme  = Either Stem Exponent
data Array     = Array [Exponent] -- String <- we don't need a label?
data Workspace = Workspace [Feature] [Array] [Morpheme]
{-
Constraints are DFSTs that target one of the three lists in the Workspace.
-}

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
        let (workspace',movee) = (work++(tail space),head space) 
        ((work',space'),movee') <- map (\x -> (x,movee)) $ split workspace'
        let final = (work'++(movee' : space'))
        guard (final /= workspace)
        return final
