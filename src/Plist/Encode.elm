module Plist.Encode exposing
    ( dict
    , dictList
    , array
    , list
    , string
    , bool
    )

import Plist.Internal exposing (Value(..))

import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)



-- VALUES


dict : Dict String Value -> Value
dict =
    Dict


dictList : List ( String, Value ) -> Value
dictList =
    Dict.fromList >> Dict


array : Array Value -> Value
array =
    Array.toList >> Array


list : List Value -> Value
list =
    Array


string : String -> Value
string =
    String


bool : Bool -> Value
bool =
    Bool
