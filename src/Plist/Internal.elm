module Plist.Internal exposing
    ( Value(..)
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
