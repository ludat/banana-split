module Generated.Moneda exposing (escalaDe)

import Generated.Api exposing (Moneda(..))


{-| Cuántos decimales tiene un monto de esta moneda.

Generado desde `BananaSplit.Moneda.escalaDe`. No editar a mano.

-}
escalaDe : Moneda -> Int
escalaDe moneda =
    case moneda of
        ARS ->
            2

        USD ->
            2

        EUR ->
            2

        BRL ->
            2

        UYU ->
            2

        CLP ->
            2

        GBP ->
            2
