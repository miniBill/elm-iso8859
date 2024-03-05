module Iso8859.Part1 exposing (decode, encode)

{-| Encode and decode strings according to the [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) encoding.

@docs decode, encode

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Iso8859


{-| -}
decodeDict : Dict Int Char
decodeDict =
    let
        ascii : List ( Int, Char )
        ascii =
            List.range 0 0x7E
                |> List.map (\i -> ( i, Char.fromCode i ))

        latin1List : List ( Int, Char )
        latin1List =
            List.range 0xA0 0xFF
                |> List.map (\i -> ( i, Char.fromCode i ))
    in
    (ascii ++ latin1List)
        |> Dict.fromList


encodeDict : Dict Char Int
encodeDict =
    Iso8859.reverseDict decodeDict


{-| Encode a string according to the [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) encoding. Returns `Nothing` if any of the characters are unsupported.
-}
encode : String -> Maybe Bytes
encode input =
    Iso8859.encode encodeDict input


{-| Decode a string according to the [ISO/IEC 8859-1](https://en.wikipedia.org/wiki/ISO/IEC_8859-1) encoding. Returns `Nothing` if any of the characters are unsupported.
-}
decode : Bytes -> Maybe String
decode bytes =
    Iso8859.decode decodeDict bytes
