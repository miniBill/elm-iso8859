# `elm-iso8859`

This package allows encoding and decoding of strings in the ISO/IEC 8859 and Windows-1252 encodings.

Currently only ISO/IEC 8859-1 and Windows-1252 are supported.

## Example

```elm
import Bytes exposing (Bytes)
import Iso8859.Part1

encoded : Maybe Bytes
encoded =
    Iso8859.Part1.fromString "Caffé"
    --> 43 61 66 66 e9
```
