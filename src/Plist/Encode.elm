module Plist.Encode exposing
    ( dict
    , dictList
    , array
    , list
    , string
    , bool
    , data
    , date
    , integer
    , real
    )

import Plist.Internal exposing (Value(..))

import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)
import Bytes exposing (Bytes)
import Time



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


data : Bytes -> Value
data =
    Data


date : Time.Posix -> Value
date =
    Date


integer : Int -> Value
integer =
    Integer


real : Float -> Value
real =
    Real
