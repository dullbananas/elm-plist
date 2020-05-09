module TestEncodeBytes exposing (suite)

import Test exposing (..)
import Fuzz as F
import Expect exposing (Expectation)

import Bytes
import Bytes.Encode
import Dict

import Plist.Binary as B
import Plist.Internal as P


suite : Test
suite =
    describe "Plist.Binary"
        [ fuzz ( F.intRange 0 65540 ) "Marker size calculation" <| \size ->
            B.encodeMarker 0 size
                |> Bytes.Encode.encode
                |> Bytes.width
                |> Expect.equal ( B.markerWidth size )

        , only <| test "Plist.Internal.depth" <| \_ ->
            complexValue
                |> P.depth
                |> Expect.equal 17

        , test "objectFromValue" <| \_ ->
            complexValue
                |> B.objectFromValue 0
                |> Expect.equal complexObject

        , test "objectFromValue dict" <| \_ ->
            ( P.Dict <| Dict.fromList [ ("a", P.Integer 0), ("b", P.Integer 4) ] )
                |> B.objectFromValue 0
                |> Expect.equal (
                    B.Object 0 <| B.Dict
                        [ (stro 1 "a", into 2 0)
                        , (stro 3 "b", into 4 4)
                        ]
                )
        ]


complexValue : P.Value
complexValue =
    P.Array
        [ P.Integer 14
        , P.Array [ P.Integer 12, P.Integer 13, P.Array [P.Integer 0, P.Integer 1] ]
        , P.Dict <| Dict.fromList
            [ ("one", P.Integer 100)
            , ("two", P.String "i miss ryl :(")
            , ("three", P.Array [ P.Integer 1000, P.Integer 1001 ])
            ]
        ]


complexObject : B.Object
complexObject =
    B.Object 0 <| B.Array
        [ into 1 14
        , B.Object 2 <| B.Array [ into 3 12, into 4 13, B.Object 5 <| B.Array [into 6 0, into 7 1] ]
        , B.Object 8 <| B.Dict
            [ (stro 9 "one", into 10 100)
            , (stro 11 "three", B.Object 12 <| B.Array [into 13 1000, into 14 1001])
            , (stro 15 "two", stro 16 "i miss ryl :(")
            ]
        ]


-- int object
into : Int -> Int -> B.Object
into ref val =
    B.Object ref <| B.Primitive <| P.Integer val


-- string object
stro : Int -> String -> B.Object
stro ref val =
    B.Object ref <| B.Primitive <| P.String val
