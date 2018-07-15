module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Physics.Space exposing (..)
import Helpers exposing (pairwiseList, distinctPairwiseList)
import Set


sun : SolarBody
sun =
    { x = 0.0e0, y = 0.0e0, velocityX = 0.0e0, velocityY = 0, mass = 1.989e30, name = "sun.gif" }


earth : SolarBody
earth =
    { x = 1.496e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 2.98e4, mass = 5.974e24, name = "earth.gif" }


suite : Test
suite =
    describe
        "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            , fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , test "Calculate the force on the sun due to gravity acting between the sun and the earth" <|
                \() ->
                    Expect.equal { fx = 3.5412994196645605e22, fy = 0 } (gravityForce sun earth)
            , test "pairwise generation" <|
                \() ->
                    Expect.equal
                        (Set.fromList (pairwiseList [ 1, 2 ] [ 3, 4 ]))
                        (Set.fromList [ ( 1, 3 ), ( 1, 4 ), ( 2, 3 ), ( 2, 4 ) ])
            , test "distinctPairwiseList generation" <|
                \() ->
                    Expect.equal
                        (Set.fromList (distinctPairwiseList [ 1, 2 ] [ 1, 2 ]))
                        (Set.fromList [ ( 1, 2 ), ( 2, 1 ) ])

            --, test "pairwise in no order" <|
            --\() ->
            --Expect.equal
            --(pairwiseList [ 1, 2 ] [ "a", "b" ])
            --[ ( 1, "b" ), ( 2, "a" ), ( 1, "a" ), ( 2, "b" ) ]
            ]
        ]
