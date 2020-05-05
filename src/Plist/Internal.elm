module Plist.Internal exposing
    ( Value(..)
    )

import Dict exposing (Dict)


type Value
    = Dict ( Dict String Value )
    | Array ( List Value )
    | String String
    | Bool Bool
