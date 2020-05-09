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

    , encodeXml
    )

import Plist.Internal exposing (Value(..))

import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)
import Bytes exposing (Bytes)
import Time
import XmlParser exposing
    ( Xml
    , Node(..)
    , Attribute
    , ProcessingInstruction
    , DocType
    )
import Base64
import Iso8601



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



-- SERIALIZE


encodeXml : Value -> Xml
encodeXml val =
    { processingInstructions =
        [ ProcessingInstruction
            "xml"
            "version=\"1.0\" encoding=\"UTF-8\""
        ]

    , docType =
        Just <| DocType
            "plist"
            ( XmlParser.Public
                "-//Apple//DTD PLIST 1.0//EN"
                "http://www.apple.com/DTDs/PropertyList-1.0.dtd"
                Nothing
            )

    , root =
        Element
            "plist"
            [ Attribute "version" "1.0" ]
            [ encodeNode val ]
    }


encodeNode : Value -> Node
encodeNode val =
    case val of
        Dict d ->
            Dict.toList d
                |> List.map (
                    \(key, dval) ->
                        [ Element "key" [] [ Text key ]
                        , encodeNode dval
                        ]
                )
                |> List.concat
                |> Element "dict" []

        Array items ->
            Element "array" [] <| List.map encodeNode items

        String v ->
            el "string" v

        Bool True ->
            Element "true" [] []

        Bool False ->
            Element "false" [] []

        Data v ->
            el "data" <| Maybe.withDefault "" <| Base64.fromBytes v

        Date v ->
            el "date" <| Iso8601.fromTime v

        Integer v ->
            el "integer" <| String.fromInt v

        Real v ->
            el "real" <| String.fromFloat v


el : String -> String -> Node
el name val =
    Element name [] [ Text val ]



{-binEncodeValue : Int -> BinValue -> B.Encoder
binEncodeValue refLength val =
    case val of
        BinDict keys vals ->
            B.sequence <|
                [ marker 0xD0 <| List.length keys ]
                ++ List.map encodeRef keys
                ++ List.map encodeRef vals
-}
