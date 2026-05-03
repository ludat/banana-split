module Utils.Posix exposing (Posix, jsonDecPosix, jsonEncPosix)

import Iso8601
import Json.Decode
import Json.Encode
import Time


type alias Posix =
    Time.Posix


jsonDecPosix : Json.Decode.Decoder Posix
jsonDecPosix =
    Iso8601.decoder


jsonEncPosix : Posix -> Json.Encode.Value
jsonEncPosix =
    Iso8601.encode
