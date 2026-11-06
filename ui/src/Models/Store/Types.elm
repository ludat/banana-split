module Models.Store.Types exposing (Store, StoreMsg(..))

import Dict exposing (Dict)
import Generated.Api exposing (Grupo, Pago, RepartijaForFrontend, ResumenGrupo, ShallowPago, Transferencia, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData Grupo)
    , resumenes : Dict ULID (WebData ResumenGrupo)
    , pagosPorGrupo : Dict ULID (WebData (List ShallowPago))
    , transferenciasPorGrupo : Dict ULID (WebData (List Transferencia))
    , repartijas : Dict ULID (WebData RepartijaForFrontend)
    , pagos : Dict ULID (WebData Pago)
    }


type StoreMsg
    = GrupoFetched ULID (WebData Grupo)
    | FetchGrupo ULID
    | ResumenFetched ULID (WebData ResumenGrupo)
    | FetchResumen ULID
    | InvalidateResumen ULID
    | PagosFetched ULID (WebData (List ShallowPago))
    | FetchPagos ULID (Maybe ULID)
    | InvalidatePagos ULID
    | TransferenciasFetched ULID (WebData (List Transferencia))
    | FetchTransferencias ULID
    | TransferenciaActualizada ULID Transferencia
    | TransferenciaBorrada ULID ULID
    | PagoFetched ULID (WebData Pago)
    | FetchPago ULID ULID
    | RepartijaFetched ULID (WebData RepartijaForFrontend)
    | FetchRepartija ULID
