module Utils.Ulid exposing (..)

import Generated.Api exposing (ULID)


emptyUlid : ULID
emptyUlid =
    "00000000000000000000000000"


toString : ULID -> String
toString =
    identity
