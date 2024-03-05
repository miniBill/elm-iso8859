module Iso8859 exposing (decode, encode, reverseDict)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Maybe.Extra


encode : Dict Char Int -> String -> Maybe Bytes
encode encodeDict input =
    input
        |> String.toList
        |> Maybe.Extra.combineMap
            (\c ->
                Dict.get c encodeDict
                    |> Maybe.map Bytes.Encode.unsignedInt8
            )
        |> Maybe.map
            (\bytes ->
                bytes
                    |> Bytes.Encode.sequence
                    |> Bytes.Encode.encode
            )


decode : Dict Int Char -> Bytes.Bytes -> Maybe String
decode decodeDict bytes =
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
                            |> Bytes.Decode.andThen
                                (\char ->
                                    case Dict.get char decodeDict of
                                        Just c ->
                                            Bytes.Decode.succeed c

                                        Nothing ->
                                            Bytes.Decode.fail
                                )
                            |> Bytes.Decode.map (\char -> Bytes.Decode.Loop ( remaining - 1, char :: acc ))
                )
    in
    Bytes.Decode.decode decoder bytes


reverseDict : Dict Int Char -> Dict Char Int
reverseDict decodeDict =
    decodeDict
        |> Dict.toList
        |> List.map (\( k, v ) -> ( v, k ))
        |> Dict.fromList
