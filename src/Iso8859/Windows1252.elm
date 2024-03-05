module Iso8859.Windows1252 exposing (decode, encode)

{-| Encode and decode strings according the [Windows-1252](https://en.wikipedia.org/wiki/Windows-1252) encoding.

@docs decode, encode

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Iso8859


specific : String
specific =
    "€.‚ƒ„…†‡ˆ‰Š‹Œ.Ž.‘’“”•–—˜™š›œ.žŸ"


decodeDict : Dict Int Char
decodeDict =
    let
        ascii : List ( Int, Char )
        ascii =
            List.range 0 0x7F
                |> List.map (\i -> ( i, Char.fromCode i ))

        specificList : List ( Int, Char )
        specificList =
            specific
                |> String.toList
                |> List.indexedMap
                    (\index c ->
                        if c /= '.' then
                            Just ( index + 0x80, c )

                        else
                            Nothing
                    )
                |> List.filterMap identity

        latin1List : List ( Int, Char )
        latin1List =
            List.range 0xA0 0xFF
                |> List.map (\i -> ( i, Char.fromCode i ))
    in
    (ascii ++ specificList ++ latin1List)
        |> Dict.fromList


encodeDict : Dict Char Int
encodeDict =
    Iso8859.reverseDict decodeDict


{-| Encode a string according to the [Windows-1252](https://en.wikipedia.org/wiki/Windows-1252) encoding. Returns `Nothing` if any of the characters are unsupported.
-}
encode : String -> Maybe Bytes
encode input =
    Iso8859.encode encodeDict input


{-| Decode a string according to the [Windows-1252](https://en.wikipedia.org/wiki/Windows-1252) encoding. Returns `Nothing` if any of the characters are unsupported.
-}
decode : Bytes -> Maybe String
decode bytes =
    Iso8859.decode decodeDict bytes
