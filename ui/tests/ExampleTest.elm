module ExampleTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Example"
        [ test "1 equals 1" <|
            \_ ->
                Expect.equal 1 1
        ]
