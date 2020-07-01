import Prelude hiding (exponent)
import HS_morpho

-- features
-- dies 
obl :: Bool -> Feature
obl = feature "obl"
gov :: Bool -> Feature
gov = Labeled "gov"
masc :: Bool -> Feature
masc = Labeled "masc"
fem :: Bool -> Feature
fem = Labeled "fem"

-- exponents
-- dies
k :: [Array]
k = pure $ array "k"
    [ exponent "m" [masc True, obl True, gov True]
    , exponent "s" [masc True, obl True]
    , exponent "s" [masc True, fem True]
    , exponent "n" [masc True, gov True]
    , exponent "r" [masc True]
    , exponent "r" [masc True]
    , exponent "n" [obl True, gov True]
    , exponent "r" [obl True]
    , exponent "e" []
    ]

-- constraints
-- dies
maxM :: Constraint
maxM = mkMax "masc"
maxO :: Constraint
maxO = mkMax "obl"
maxF :: Constraint
maxF = mkMax "fem"
maxG :: Constraint
maxG = mkMax "gov"

-- grammars
-- dies
testDies :: [Feature] -> String
testDies features = getMorphemes $ converge dies (Workspace features k [Left $ stem "dies" D])

dies :: (GEN, Ranking)
dies = (diesGen,diesRanking) 
  where
    diesGen :: GEN
    diesGen = mkGen True False
    diesRanking :: Ranking
    diesRanking = 
        [ mc
        , idF
        , maxM
        , maxO
        , maxF
        , maxG
        ]
