module Models.MontoTest exposing (suite)

import Expect
import Generated.Api exposing (Monto)
import Models.Monto as Monto
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Monto"
        [ describe "toString"
            [ test "Monto 2 100050" <| \_ -> Expect.equal "1.000,50" (Monto.toString (Monto 2 100050))
            , test "Monto 0 0" <| \_ -> Expect.equal "0" (Monto.toString (Monto 0 0))
            , test "Monto 2 50" <| \_ -> Expect.equal "0,50" (Monto.toString (Monto 2 50))
            , test "Monto 2 -100050" <| \_ -> Expect.equal "-1.000,50" (Monto.toString (Monto 2 -100050))
            , test "Monto 2 100000" <| \_ -> Expect.equal "1.000,00" (Monto.toString (Monto 2 100000))
            ]
        , describe "add"
            [ test "same precision" <|
                \_ -> Expect.equal (Monto 2 150) (Monto.add (Monto 2 100) (Monto 2 50))
            , test "scales to the larger precision" <|
                \_ -> Expect.equal (Monto 2 150) (Monto.add (Monto 0 1) (Monto 2 50))
            , test "keeps precision of more precise operand" <|
                \_ -> Expect.equal (Monto 3 1500) (Monto.add (Monto 1 5) (Monto 3 1000))
            , test "is commutative" <|
                \_ -> Expect.equal (Monto.add (Monto 2 50) (Monto 0 1)) (Monto.add (Monto 0 1) (Monto 2 50))
            , test "handles negatives" <|
                \_ -> Expect.equal (Monto 2 -50) (Monto.add (Monto 0 1) (Monto 2 -150))
            ]
        ]
