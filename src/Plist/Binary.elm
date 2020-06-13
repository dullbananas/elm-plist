module Plist.Binary exposing
    ( SizeConfig
    , Object
    , Value(..)

    , objectFromValue

    , encodeMarker
    , markerWidth
    )


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as E exposing (Encoder)
import Bitwise
import Time
import Dict
import Cons exposing (Cons(..))

import Plist.Internal as P

{-| This module contains internal stuff used for binary plists.
-}


type alias Object =
    { ref : Int
    , value : Value
    }


{-| Not to be confused with `P.Value`.
-}

type Value
    = Dict ( List (Int, Int) )
    | Array ( List Int )
    | Primitive P.Value


objectsFromValue : Int -> P.Value -> Cons Object
objectsFromValue rootRef value =
    case value of
        P.Dict dict ->
            --
            Dict <| .entries <|
                List.foldl
                    ( \(key, item) state ->
                        { nextRef = state.nextRef + P.depth item + 1
                        , entries = state.entries ++
                            [(Object state.nextRef <| Primitive <| P.String key
                            , objectFromValue (state.nextRef + 1) item
                            )]
                        }
                    )
                    { nextRef = rootRef + 1, entries = [] }
                    ( Dict.toList dict )

        P.Array items ->
            let
                objects : List ( Cons Object )
                objects =
                    .result <| List.foldl
                        ( \item { result, nextRef } ->
                            { nextRef = state.nextRef + P.depth item
                            , result = state.result ++ [ ob ]
                            }
                        )
                refs : List Int
                refs =
                    List.map ( Cons.head >> .ref ) objects
            in
                Cons
                    ( Array refs )
                    ( objects )
            --
            Array <| .objects <|
                List.foldl
                    ( \item state ->
                        { nextRef = state.nextRef + P.depth item
                        , objects = state.objects ++ [ objectFromValue state.nextRef item ]
                        }
                    )
                    { nextRef = rootRef + 1, objects = [] }
                    items

        primitive ->
            Cons.singleton <| Primitive primitive


{-| Encode for a marker, which indicates the type of object and the size. The
first argument is 0xD0 for dictionary, 0x40 for data, etc.
-}

encodeMarker : Int -> Int -> Encoder
encodeMarker type_ size =
    if size < 16 then
        E.unsignedInt8 <| Bitwise.or type_ size
    else
        E.sequence
            [ E.unsignedInt8 <| Bitwise.or type_ 0x0F
            , E.unsignedInt8 <| Bitwise.or 0x10 <| intWidthPower size
            , case intWidthPower size of
                0 -> E.unsignedInt8 size
                1 -> E.unsignedInt16 BE size
                2 -> E.unsignedInt32 BE size
                3 -> Debug.todo "Support for 64 bit size in encodeMarker"
                _ -> E.unsignedInt32 BE size
            ]


encodeRef : SizeConfig -> Object -> Encoder
encodeRef { refSize } =
    .ref >> case refSize of
        1 ->
            E.unsignedInt8
        _ ->
            E.unsignedInt16 BE


{-| Contains configuration for the sizes of references, offsets, etc. measured
in bytes. This is stored in the trailer, which is the last section of a binary
plist file.
-}

type alias SizeConfig =
    { offsetSize : Int
    , refSize : Int
    }


encodeValue : SizeConfig -> Value -> Encoder
encodeValue sizeConf value =
    case value of
        Dict items ->
            E.sequence
                [ encodeMarker 0xD0 <| List.length items
                , E.sequence
                    ( items |> List.map Tuple.first |> List.map (encodeRef sizeConf) )
                , E.sequence
                    ( items |> List.map Tuple.second |> List.map (encodeRef sizeConf) )
                ]

        Array items ->
            E.sequence
                [ encodeMarker 0xA0 <| List.length items
                , E.sequence <| List.map ( encodeRef sizeConf ) items
                ]

        Primitive primitive ->
            case primitive of
                P.String string ->
                    E.sequence
                        [ encodeMarker 0x50 <| E.getStringWidth string
                        , E.string string
                        ]

                P.Bool False ->
                    E.unsignedInt8 0x08

                P.Bool True ->
                    E.unsignedInt8 0x09

                P.Data data ->
                    E.sequence
                        [ encodeMarker 0x40 <| Bytes.width data
                        , E.bytes data
                        ]

                P.Date time ->
                    E.sequence
                        [ E.unsignedInt8 0x331
                        , E.float64 BE <| ( toFloat <| Time.posixToMillis time ) / 1000
                        ]

                P.Integer int ->
                    E.sequence
                        [ encodeMarker 0x10 <| intWidthPower int
                        , case intWidthPower int of
                            0 -> E.signedInt8 int
                            1 -> E.signedInt16 BE int
                            2 -> E.signedInt32 BE int
                            3 -> Debug.todo "Support 64 bit int size in encodeValue"
                            _ -> E.signedInt32 BE int
                        ]

                P.Real float ->
                    E.sequence
                        [ E.unsignedInt8 0x23
                        , E.float64 BE float
                        ]

                _ ->
                    E.sequence [] -- Should never happen


{-| Calculates the size of a `Value` in bytes. If it is an array or a
dictionary, the size does not include the actual data of the contents; only
the references. This includes the marker at the beginning which indicates the
type of value and sometimes the length.
-}

valueWidth : SizeConfig -> Value -> Int
valueWidth { refSize } value =
    case value of
        Dict items ->
            widthHelper (2 * refSize) <| List.length items

        Array items ->
            widthHelper refSize <| List.length items

        Primitive primitive ->
            case primitive of
                P.String string ->
                    widthHelper 1 <| E.getStringWidth string

                P.Bool _ ->
                    1

                P.Data data ->
                    widthHelper 1 <| Bytes.width data

                P.Date time ->
                    9

                P.Integer int ->
                    1 + intWidth int

                P.Real float ->
                    9 -- This could be variable sized just like Integer but I don't know how floats work

                _ ->
                    0 -- Should never happen


widthHelper : Int -> Int -> Int
widthHelper bytesPerItem size =
    ( markerWidth size ) + ( size * bytesPerItem )


markerWidth : Int -> Int
markerWidth length =
    if length < 16 then
        1
    else
        2 + intWidth length


intWidth : Int -> Int
intWidth int =
    2 ^ ( intWidthPower int )


{- Calculates the value of n when the size of the given integer needs 2^n
bytes to be stored. Use 2^(intWidth x) to get the actual byte length of x.
-}

intWidthPower : Int -> Int
intWidthPower int =
    if int < 256 then
        0
    else if int < 65536 then
        1
    else if int < 4294967296 then
        2
    else
        Debug.todo "Support 64 bit lengths"
