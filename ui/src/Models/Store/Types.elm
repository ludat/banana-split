module Models.Store.Types exposing (Store, StoreMsg(..))

import Dict exposing (Dict)
import Generated.Api exposing (MetricasGrupo, Pago, RepartijaForFrontend, ResumenGrupo, ShallowGrupo, ShallowPago, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData ShallowGrupo)
    , resumenes : Dict ULID (WebData ResumenGrupo)
    , metricas : Dict ULID (WebData MetricasGrupo)
    , pagosPorGrupo : Dict ULID (WebData (List ShallowPago))
    , repartijas : Dict ULID (WebData RepartijaForFrontend)
    , pagos : Dict ULID (WebData Pago)
    }


type StoreMsg
    = GrupoFetched ULID (WebData ShallowGrupo)
    | FetchGrupo ULID
    | ResumenFetched ULID (WebData ResumenGrupo)
    | FetchResumen ULID
    | InvalidateResumen ULID
    | MetricasFetched ULID (WebData MetricasGrupo)
    | FetchMetricas ULID
    | PagosFetched ULID (WebData (List ShallowPago))
    | FetchPagos ULID
    | InvalidatePagos ULID
    | PagoFetched ULID (WebData Pago)
    | FetchPago ULID
    | RepartijaFetched ULID (WebData RepartijaForFrontend)
    | FetchRepartija ULID
