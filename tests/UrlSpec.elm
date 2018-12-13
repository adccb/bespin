module UrlSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict exposing (fromList)
import Url exposing (..)

suite : Test
suite =
  describe "URL" [
    describe "fromString" [
      test "works" <|
        let
          urlString = "https://test.co/example/?query1=foo"
          url : Url
          url = 
            {
              raw = urlString,
              protocol = "https",
              queryString = Dict.fromList [("query1", "foo")]
            } 
        in
          \_ ->
            urlString
              |> fromString
              |> Expect.equal url
    ]  
  ]
