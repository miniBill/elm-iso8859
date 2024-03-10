module Iso8859.Part1 exposing (toString, fromString)

{-| Encode and decode strings according to the [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) encoding.

@docs toString, fromString

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Iso8859


{-| -}
decodeDict : Dict Int Char
decodeDict =
    let
        ascii : List Int
        ascii =
            List.range 0 0x7E

        latin1 : List Int
        latin1 =
            List.range 0xA0 0xFF
    in
    (ascii ++ latin1)
        |> List.map (\i -> ( i, Char.fromCode i ))
        |> Dict.fromList


encodeDict : Dict Char Int
encodeDict =
    Iso8859.reverseDict decodeDict


{-| Encode a string according to the [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) encoding. Returns `Nothing` if any of the characters are unsupported.
-}
fromString : String -> Maybe Bytes
fromString input =
    Iso8859.fromString encodeDict input


{-| Decode a string according to the [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) encoding. Returns `Nothing` if any of the characters are unsupported.
-}
toString : Bytes -> Maybe String
toString bytes =
    Iso8859.toString decodeDict bytes
