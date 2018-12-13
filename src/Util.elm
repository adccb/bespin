module Util exposing (..)

import Dict exposing (..)
import Regex

queryStringRegex = Regex.fromString "\\?([A-z0-9]+=[A-z0-9]+)+$"
isEven n = (remainderBy n 2) == 0
isOdd n = not (isEven n)
map f a = List.map f a
filter f a = List.filter f a
reduce f i a = List.foldl f i a

-- elm's List.concat only supports a depth of 1; that is, `List List a`
-- is fine, but `List List List b` throws a type error. this recursive
-- flatten has a depth of Infinity.
type ListOrValue t = AList (List (ListOrValue t)) | AValue t
flatten : List (ListOrValue a) -> List a
flatten a =
  List.foldr
    (
      \itm acc ->
        case itm of
          AList lst -> 
            List.append (flatten lst) acc
          AValue val ->
            val :: acc
    )
    []
    a

flatMap f a =
  a
    |> flatten
    |> map f

groupBy : (comparable -> comparable) -> List comparable -> Dict comparable (List comparable) 
groupBy f a =
  let
      dict : Dict comparable (List comparable)
      dict = Dict.empty
  in
    List.foldr
      (
        \itm acc ->
          let
            result = f itm
            entry : Maybe (List comparable)
            entry = Dict.get result acc
          in
            case entry of
              Nothing ->
                Dict.insert result [ itm ] acc
              Just val ->
                Dict.insert result (itm :: val) acc
      )
      dict
      a

queryStringToHash : String -> Dict String String
queryStringToHash str =
  str
    |> (
      \x ->
        if String.startsWith "?" x then 
          (String.dropLeft 1 x) 
        else 
          x
      )
    |> String.split "&"
    |> List.foldl
      (\itm acc ->
        case String.split "=" itm of
          [key, value] ->
            Dict.insert key value acc
          _ ->
            acc
      )
      Dict.empty

