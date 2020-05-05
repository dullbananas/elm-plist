module Plist.Decode exposing
    ( Decoder
    , Error(..)
    , decodeValue
    , value
    , string
    , bool
    , list
    , field
    , succeed
    , fail
    , andThen
    , lazy
    , map2
    )

import Result.Extra as ResultE
import Dict exposing (Dict)

import Plist.Internal exposing (Value(..))



-- TYPES


type Decoder a
    = Decoder ( Value -> Result Error a )


type Error
    = WrongType Value
    | MissingField String Value
    | Failure String Value



-- FUNCTIONS


decodeValue : Decoder a -> Value -> Result Error a
decodeValue ( Decoder decode ) =
    decode



-- DECODERS


value : Decoder Value
value =
    Decoder Ok


string : Decoder String
string =
    Decoder <| \val ->
        case val of
            String v ->
                Ok v
            _ ->
                Err <| WrongType val


bool : Decoder Bool
bool =
    Decoder <| \val ->
        case val of
            Bool v ->
                Ok v
            _ ->
                Err <| WrongType val


list : Decoder a -> Decoder ( List a )
list ( Decoder decode ) =
    Decoder <| \val ->
        case val of
            Array items ->
                items
                    |> List.map decode
                    |> ResultE.combine
            _ ->
                Err <| WrongType val



-- ADVANCED


field : String -> Decoder a -> Decoder a
field name ( Decoder decode ) =
    Decoder <| \val ->
        case val of
            Dict d ->
                Dict.get name d
                    |> Result.fromMaybe ( MissingField name val )
                    |> Result.andThen decode
            _ ->
                Err <| WrongType val


succeed : a -> Decoder a
succeed val =
    Decoder <| always <| Ok val


fail : String -> Decoder a
fail message =
    Decoder <| \val -> Err <| Failure message val


andThen : ( a -> Decoder b ) -> Decoder a -> Decoder b
andThen callback ( Decoder decode ) =
    Decoder <| \val ->
        case decode val of
            Ok result ->
                case callback result of
                    Decoder cb -> cb val
            Err err ->
                Err err


lazy : (() -> Decoder a) -> Decoder a -- "borrowed" from elm/json
lazy thunk =
    andThen thunk (succeed ())


map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 constructor ( Decoder decode0 ) ( Decoder decode1 ) =
    Decoder <| \val ->
        case ( decode0 val, decode1 val ) of
            ( Ok arg0, Ok arg1 ) ->
                Ok <| constructor arg0 arg1

            ( Err err, _ ) ->
                Err err

            ( _, Err err ) ->
                Err err
