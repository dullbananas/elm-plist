module Tests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)

import Dict

import Plist.Internal exposing (Value(..))
import Plist.Encode as E
import Plist.Decode as D
import Plist.Decode.Pipeline as P
import Plist


suite : Test
suite =
    describe "Plist"
        [ test "Encode" <| \_ ->
            E.dictList
                [ ("bestPersonEver", E.string "Evan Czaplicki")
                , ("worstLang", E.string "JavaScript")
                , ("javascriptIsGood", E.bool False)
                , ("people", E.list
                    [ encodeName <| Name "Dul" "Bana"
                    , encodeName <| Name "Patrick" "Enis"
                    ] )
                ]
                |> Expect.equal plist0

        , describe "Decode"
            [ test "string" <| \_ ->
                String "I <3 ryl"
                    |> D.decodeValue D.string
                    |> Expect.equal ( Ok "I <3 ryl" )

            , test "string (wrong type)" <| \_ ->
                Bool True
                    |> D.decodeValue D.string
                    |> Expect.equal ( Err <| D.WrongType <| Bool True )

            , test "bool" <| \_ ->
                Bool True
                    |> D.decodeValue D.bool
                    |> Expect.equal ( Ok True )

            , test "value" <| \_ ->
                String "I miss ryl"
                    |> D.decodeValue D.value
                    |> Expect.equal ( Ok <| String "I miss ryl" )

            , test "string list" <| \_ ->
                Array [ String "I", String "love", String "Ryl" ]
                    |> D.decodeValue ( D.list D.string )
                    |> Expect.equal ( Ok [ "I", "love", "Ryl" ] )

            , test "field" <| \_ ->
                ( Dict <| Dict.fromList [("foo", String "<3"), ("bar", String "ryl")] )
                    |> D.decodeValue ( D.field "foo" D.string )
                    |> Expect.equal ( Ok "<3" )

            , test "succeed" <| \_ ->
                String "thing"
                    |> D.decodeValue ( D.succeed "Boomer" )
                    |> Expect.equal ( Ok "Boomer" )

            , test "fail" <| \_ ->
                String "AAAAAAAA"
                    |> D.decodeValue ( D.fail "Too much screaming" )
                    |> Expect.equal ( Err <| D.Failure "Too much screaming" <| String "AAAAAAAA" )

            , test "andThen" <| \_ ->
                String "AAA"
                    |> D.decodeValue
                        ( D.string |> D.andThen (\text -> if text == "AAA" then D.succeed 40 else D.fail "no") )
                    |> Expect.equal ( Ok 40 )

            , test "map2" <| \_ ->
                ( Dict <| Dict.fromList [("first", String "Evan"), ("last", String "Czaplicki")] )
                    |> D.decodeValue
                        ( D.map2 Name
                            ( D.field "first" D.string )
                            ( D.field "last" D.string )
                        )
                    |> Expect.equal ( Ok <| Name "Evan" "Czaplicki" )

            , describe "Pipeline"
                [ test "required" <| \_ ->
                    ( Dict <| Dict.fromList
                        [ ("city", String "Sacramento")
                        , ("state", String "California")
                        , ("country", String "United States")
                        ]
                    ) |> D.decodeValue
                        ( D.succeed Place
                            |> P.required "city" D.string
                            |> P.required "state" D.string
                            |> P.required "country" D.string
                    ) |> Expect.equal
                        ( Ok <| Place "Sacramento" "California" "United States" )
                ]
            ]
        ]


type alias Place =
    { city : String
    , state : String
    , country : String
    }


type alias Name =
    { first : String
    , last : String
    }

encodeName : Name -> Plist.Value
encodeName name =
    E.dict <| Dict.fromList
        [ ("firstName", E.string name.first)
        , ("lastName", E.string name.last)
        ]


plist0 : Plist.Value
plist0 =
    Dict <| Dict.fromList
        [ ("bestPersonEver", String "Evan Czaplicki")
        , ("worstLang", String "JavaScript")
        , ("javascriptIsGood", Bool False)
        , ("people", Array
            [ Dict <| Dict.fromList [("firstName", String "Dul"), ("lastName", String "Bana")]
            , Dict <| Dict.fromList [("firstName", String "Patrick"), ("lastName", String "Enis")]
            ] )
        ]
