module Plist.Internal exposing
    ( Value(..)
    , depth
    )

import Dict exposing (Dict)
import Bytes exposing (Bytes)
import Time


type Value
    = Dict ( Dict String Value )
    | Array ( List Value )
    | String String
    | Bool Bool
    | Data Bytes
    | Date Time.Posix
    | Integer Int
    | Real Float


{-| Used by Plist.Binary
-}

depth : Value -> Int
depth value =
    case value of
        Dict items ->
            Dict.toList items
                |> List.map ( Tuple.second >> depth )
                |> List.sum -- the values
                |> (+) ( Dict.size items ) -- the keys
                |> (+) 1 -- the dict itself

        Array items ->
            items
                |> List.map depth
                |> List.sum -- the items
                |> (+) 1 -- the array itself

        _ ->
            1
