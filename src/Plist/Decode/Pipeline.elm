module Plist.Decode.Pipeline exposing
    ( custom
    , required
    )

import Plist.Decode as D exposing (Decoder)

-- this module is "borrowed" from Json.Decode.Pipeline


custom : Decoder a -> Decoder ( a -> b ) -> Decoder b
custom =
    D.map2 (|>)


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
    custom (D.field key valDecoder) decoder
