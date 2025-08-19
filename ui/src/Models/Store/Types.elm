module Models.Store.Types exposing (..)

import Dict exposing (Dict)
import Generated.Api exposing (Grupo, Pago, Repartija, ResumenGrupo, ShallowGrupo, ShallowPago, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData ShallowGrupo)
    , resumenes : Dict ULID (WebData ResumenGrupo)
    , pagosPorGrupo : Dict ULID (WebData (List ShallowPago))
    , repartijas : Dict ULID (WebData Repartija)
    , pagos : Dict ULID (WebData Pago)
    }


type StoreMsg
    = GrupoFetched ULID (WebData ShallowGrupo)
    | FetchGrupo ULID
    | ResumenFetched ULID (WebData ResumenGrupo)
    | FetchResumen ULID
    | PagosFetched ULID (WebData (List ShallowPago))
    | FetchPagos ULID
    | PagoFetched ULID (WebData Pago)
    | FetchPago ULID
    | RepartijaFetched ULID (WebData Repartija)
    | FetchRepartija ULID
    | StoreNoOp String
