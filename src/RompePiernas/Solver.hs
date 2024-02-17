module RompePiernas.Solver (main2) where

import Prelude hiding ((<>))

import Numeric.LinearProgramming
import Numeric.LinearAlgebra



-- prob = Maximize [4, -3, 2]
coso =
    matrix 3 
    [  0,  1, 12
    , 82,  0,  0 
    ,  2, 21,  0
    ]

-- >>> (tr coso - coso) #> (vector [1,1,1])
-- Couldn't match expected type: Vector R
--             with actual type: Matrix Double
-- In the second argument of `(#>)', namely `(row [1, 1, 1])'
-- In the expression: (tr coso - coso) #> (row [1, 1, 1])
-- In an equation for `it_akMd':
--     it_akMd = (tr coso - coso) #> (row [1, 1, 1])

prob = 
    [ 1  -- 1 -> 2
    , 12 -- 1 -> 3
    , 82 -- 2 -> 1
    , 0  -- 2 -> 3
    , 2  -- 3 -> 1
    , 21 -- 3 -> 2
    ]

-- constr1 = Sparse [ [1#1, 1#2] :<=: 10
--                  , [1#2, 5#3] :<=: 20
--                  ]
constr1 = [ [1, 1, -1, 0, -1, 0] :==: (prob !! 0 + prob !! 1 - prob !! 2 - prob !! 4) -- neto de 1
          , [-1, 0, 1, 1, 0, -1] :==: (prob !! 2 + prob !! 3 - prob !! 0 - prob !! 5) -- neto de 2
          , [0, -1, 0, -1, 1, 1] :==: (prob !! 4 + prob !! 5 - prob !! 1 - prob !! 3) -- neto de 3
          ]

{-
|   |  1 |  2 |  3 |
|---+----+----+----|
| 1 |  0 |  0 | 10 | = 10 - 81 = 
| 2 | 81 |  0 |  0 | = 81 - 22 = 
| 3 |  0 | 21 |  0 | = 23 - 12 = 
|   | 81 | 21 | 12 | = 23
-}
main2 :: IO ()
main2 = do
    print "start"
    print constr1
    print $ simplex (Minimize prob) (Dense constr1) []
    print "end"