module Iso8859.Windows1252 exposing (decode, encode)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Maybe.Extra


latin1Supplement : String
latin1Supplement =
    """€.‚ƒ„…†‡ˆ‰Š‹Œ.Ž.‘’“”•–—˜™š›œ.žŸ\u{00A0}¡¢£¤¥¦§¨©ª«¬\u{00AD}®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"""


decodeDict : Dict Int Char
decodeDict =
    let
        ascii : List ( Int, Char )
        ascii =
            List.range 0 0x7F
                |> List.map (\i -> ( i, Char.fromCode i ))

        latin1List : List ( Int, Char )
        latin1List =
            latin1Supplement
                |> String.toList
                |> List.indexedMap
                    (\index c ->
                        if c /= '.' then
                            Just ( index + 0x80, c )

                        else
                            Nothing
                    )
                |> List.filterMap identity
    in
    (ascii ++ latin1List)
        |> Dict.fromList


encodeDict : Dict Char Int
encodeDict =
    decodeDict
        |> Dict.toList
        |> List.map (\( k, v ) -> ( v, k ))
        |> Dict.fromList


encode : String -> Maybe Bytes
encode input =
    input
        |> String.toList
        |> Maybe.Extra.combineMap encodeChar
        |> Maybe.map
            (\bytes ->
                bytes
                    |> Bytes.Encode.sequence
                    |> Bytes.Encode.encode
            )


encodeChar : Char -> Maybe Bytes.Encode.Encoder
encodeChar c =
    Dict.get c encodeDict
        |> Maybe.map Bytes.Encode.unsignedInt8


decode : Bytes -> Maybe String
decode bytes =
    let
        decoder : Bytes.Decode.Decoder String
        decoder =
            Bytes.Decode.loop ( Bytes.width bytes, [] )
                (\( remaining, acc ) ->
                    if remaining <= 0 then
                        acc
                            |> List.reverse
                            |> String.fromList
                            |> Bytes.Decode.Done
                            |> Bytes.Decode.succeed

                    else
                        Bytes.Decode.unsignedInt8
                            |> Bytes.Decode.andThen decodeChar
                            |> Bytes.Decode.map (\char -> Bytes.Decode.Loop ( remaining - 1, char :: acc ))
                )
    in
    Bytes.Decode.decode decoder bytes


decodeChar : Int -> Bytes.Decode.Decoder Char
decodeChar char =
    case Dict.get char decodeDict of
        Nothing ->
            Bytes.Decode.fail

        Just c ->
            Bytes.Decode.succeed c
