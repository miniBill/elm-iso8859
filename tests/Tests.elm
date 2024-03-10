module Tests exposing (part1, windows)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Fuzz exposing (Fuzzer)
import Hex.Convert
import Iso8859.Part1
import Iso8859.Windows1252
import Test exposing (Test, describe, fuzz)


iso8859_1 : String
iso8859_1 =
    -- The initial space is intentional
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u{00A0}¡¢£¤¥¦§¨©ª«¬\u{00AD}®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"


windows1252 : String
windows1252 =
    -- The initial space is intentional
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u{007F}€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ\u{00A0}¡¢£¤¥¦§¨©ª«¬\u{00AD}®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"


part1 : Test
part1 =
    describe "ISO/IEC 8859-1"
        [ fuzz Fuzz.asciiString "Is the same as UTF-8 on ASCII" <|
            \input ->
                input
                    |> Iso8859.Part1.fromString
                    |> Expect.equal (Just <| Bytes.Encode.encode <| Bytes.Encode.string input)
        , fuzz Fuzz.asciiString "It can encode ASCII strings" <|
            \input ->
                input
                    |> Iso8859.Part1.fromString
                    |> Expect.notEqual Nothing
        , fuzz (fuzzer iso8859_1) "It can encode included characters" <|
            \input ->
                input
                    |> Iso8859.Part1.fromString
                    |> Expect.notEqual Nothing
        , fuzz Fuzz.asciiString "It roundtrips on ASCII strings" <|
            \input ->
                input
                    |> Iso8859.Part1.fromString
                    |> Maybe.andThen Iso8859.Part1.toString
                    |> Expect.equal (Just input)
        , fuzz (fuzzer iso8859_1) "It roundtrips on included characters" <|
            \input ->
                let
                    encoded : Maybe Bytes
                    encoded =
                        input
                            |> Iso8859.Part1.fromString

                    decoded : Maybe String
                    decoded =
                        encoded
                            |> Maybe.andThen Iso8859.Part1.toString

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


windows : Test
windows =
    describe "Windows-1252"
        [ fuzz Fuzz.asciiString "Is the same as UTF-8 on ASCII" <|
            \input ->
                input
                    |> Iso8859.Windows1252.fromString
                    |> Expect.equal (Just <| Bytes.Encode.encode <| Bytes.Encode.string input)
        , fuzz Fuzz.asciiString "It can encode ASCII strings" <|
            \input ->
                input
                    |> Iso8859.Windows1252.fromString
                    |> Expect.notEqual Nothing
        , fuzz (fuzzer windows1252) "It can encode included characters" <|
            \input ->
                input
                    |> Iso8859.Windows1252.fromString
                    |> Expect.notEqual Nothing
        , fuzz (fuzzer iso8859_1) "It's the same as ISO8859-1 on characters supported by both" <|
            \input ->
                input
                    |> Iso8859.Windows1252.fromString
                    |> Expect.equal (Iso8859.Part1.fromString input)
        , fuzz Fuzz.asciiString "It roundtrips on ASCII strings" <|
            \input ->
                input
                    |> Iso8859.Windows1252.fromString
                    |> Maybe.andThen Iso8859.Windows1252.toString
                    |> Expect.equal (Just input)
        , fuzz (fuzzer windows1252) "It roundtrips on included characters" <|
            \input ->
                let
                    encoded : Maybe Bytes
                    encoded =
                        input
                            |> Iso8859.Windows1252.fromString

                    decoded : Maybe String
                    decoded =
                        encoded
                            |> Maybe.andThen Iso8859.Windows1252.toString

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
