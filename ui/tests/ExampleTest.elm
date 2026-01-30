module ExampleTest exposing (..)

import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Example"
        [ test "1 equals 1" <|
            \_ ->
                Expect.equal 1 1
        ]
