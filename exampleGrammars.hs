import HS_morpho

-- features
-- dies 
obl :: Bool -> Feature
obl = Binary "obl"
gov :: Bool -> Feature
gov = Binary "gov"
masc :: Bool -> Feature
masc = Binary "masc"
fem :: Bool -> Feature
fem = Binary "fem"

-- exponents
-- dies
k :: [Array]
k = pure
    [ Exponent "m" [masc True, obl True, gov True]
    , Exponent "s" [masc True, obl True]
    , Exponent "s" [masc True, fem True]
    , Exponent "n" [masc True, gov True]
    , Exponent "r" [masc True]
    , Exponent "r" [masc True]
    , Exponent "n" [obl True, gov True]
    , Exponent "r" [obl True]
    , Exponent "e" []
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
testDies features = getMorphemes $ converge dies (Workspace features k [Left $ Stem "dies" D])

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
