module UtilSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict exposing (fromList)
import Util exposing (..)

suite : Test
suite =
  describe "Util" [
    describe "isEven" [
      test "even numbers" <|
        \_ ->
          2
            |> isEven
            |> Expect.equal True

        , test "odd numbers" <|
          \_ ->
            3
              |> isEven
              |> Expect.equal False
    ],
    
    describe "isOdd" [
      test "even numbers" <|
        \_ ->
          2
            |> isOdd
            |> Expect.equal False

        , test "odd numbers" <|
          \_ ->
            3
              |> isOdd
              |> Expect.equal True
    ],
    describe "map" [
      test "mapping over a list" <| 
        \_ ->
          [ 1, 2, 3, 4, 5 ]
            |> map (\x -> x + 1)
            |> Expect.equal [ 2, 3, 4, 5, 6 ]
    ],
    describe "filter" [
      test "filtering a list" <| 
        \_ ->
          [ 1, 2, 3, 4, 5 ]
            |> filter (\x -> x > 2)
            |> Expect.equal [ 3, 4, 5 ]
    ],
    describe "reduce" [
      test "sum a list" <| 
        \_ ->
          [ 1, 2, 3, 4, 5 ]
            |> reduce (+) 0
            |> Expect.equal 15
    ],
    describe "flatten" [
      test "flattens a list" <| 
        \_ ->
          [ AList ([AValue 1, AValue 2, AList ([ AList ([ AValue 3 ]) ]) ]) ]
            |> flatten
            |> Expect.equal [ 1, 2, 3 ]
    ],
    describe "flatMap" [
      test "flatMaps a list" <| 
        \_ ->
          [ AList ([AValue 1, AValue 2, AList ([ AList ([ AValue 3 ]) ]) ]) ]
            |> flatMap (\x -> x+ 1)
            |> Expect.equal [ 2, 3, 4 ]
    ],
    describe "groupBy" [
      test "groups a list on a function" <| 
        \_ ->
          let
            list = [ 1, 2, 3, 4, 5, 6 ]
            fn = \x -> remainderBy 2 x
          in
            list
              |> groupBy fn
              |> Expect.equal (Dict.fromList [(0, [ 2, 4, 6 ]), (1, [ 1, 3, 5 ])])
    ],
    describe "queryStringToHash" [
      describe "query string present" [
        test "creates a hash from a query string" <| 
          \_ ->
            let
              queryString = "foo=bar&one=1&two=4hi5-contains-non-alphanumeric-CH4rac73r5" 
              correct = Dict.fromList
                [
                  ("foo", "bar"),
                  ("one", "1"),
                  ("two", "4hi5-contains-non-alphanumeric-CH4rac73r5")
                ]
            in
              queryStringToHash queryString
                |> Expect.equal correct
      ],
      describe "query string absent" [
        test "returns empty hash if no query string" <|
          \_ ->
            let
              queryString = ""
              correct = Dict.empty
            in
              queryStringToHash queryString
                |> Expect.equal correct
      ]

    ]
  ]
