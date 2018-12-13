module Url exposing (..)

import Regex
import Dict exposing (Dict)

import Util

type alias Url = 
  {
    raw : String,
    protocol: String,
    queryString: Dict String String
  }

protocol str = 
  case Regex.fromString "^https://" of
    Nothing ->
      "oops"
    Just r ->
      if Regex.contains r str then
        "https"
      else
        "http"

queryString : String -> Dict String String
queryString str =
  case Util.queryStringRegex of
    Nothing ->
      Dict.empty
    Just r ->
      case Regex.find r str of
        [itm] ->
          Util.queryStringToHash itm.match
        _ ->
          Dict.empty

fromString : String -> Url
fromString str =
  { 
    raw = str,
    protocol = protocol str,
    queryString = queryString str
  }

