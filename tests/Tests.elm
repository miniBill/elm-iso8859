module Tests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Fuzz exposing (Fuzzer)
import Hex.Convert
import ISO8859_1
import Test exposing (Test, describe, fuzz)


iso8859_1 : String
iso8859_1 =
    -- The initial space is intentional
    """ !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬\u{00AD}®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"""


suite : Test
suite =
    describe "ISO/IEC 8859-1"
        [ fuzz Fuzz.asciiString "Is the same as UTF-8 on ASCII" <|
            \input ->
                input
                    |> ISO8859_1.encode
                    |> Expect.equal (Just <| Bytes.Encode.encode <| Bytes.Encode.string input)
        , fuzz Fuzz.asciiString "It can encode ASCII strings" <|
            \input ->
                input
                    |> ISO8859_1.encode
                    |> Expect.notEqual Nothing
        , fuzz (fuzzer iso8859_1) "It can encode included characters" <|
            \input ->
                input
                    |> ISO8859_1.encode
                    |> Expect.notEqual Nothing
        , fuzz Fuzz.asciiString "It roundtrips on ASCII strings" <|
            \input ->
                input
                    |> ISO8859_1.encode
                    |> Maybe.andThen ISO8859_1.decode
                    |> Expect.equal (Just input)
        , fuzz (fuzzer iso8859_1) "It roundtrips on included characters" <|
            \input ->
                let
                    encoded : Maybe Bytes
                    encoded =
                        input
                            |> ISO8859_1.encode

                    decoded : Maybe String
                    decoded =
                        encoded
                            |> Maybe.andThen ISO8859_1.decode

                    _ =
                        if decoded /= Just input then
                            Debug.log "Encoded as"
                                (Maybe.map Hex.Convert.toString encoded)

                        else
                            Just ""
                in
                decoded
                    |> Expect.equal (Just input)
        ]


fuzzer : String -> Fuzzer String
fuzzer charset =
    let
        charsetList : List Char
        charsetList =
            String.toList charset

        charFuzzer : Fuzzer Char
        charFuzzer =
            Fuzz.oneOfValues charsetList
    in
    Fuzz.listOfLengthBetween 0 10 charFuzzer
        |> Fuzz.map String.fromList