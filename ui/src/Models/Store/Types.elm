module Models.Store.Types exposing (Store, StoreMsg(..))

import Dict exposing (Dict)
import Generated.Api exposing (Grupo, Pago, RepartijaForFrontend, ResumenGrupo, ShallowPago, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData Grupo)
    , resumenes : Dict ULID (WebData ResumenGrupo)
    , pagosPorGrupo : Dict ULID (WebData (List ShallowPago))
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
      -- El segundo campo es el participante que está mirando: el backend
      -- recorta el resumen de cada gasto a esa persona. 'Nothing' trae el de
      -- todos, que es lo que hay que pedir si todavía no se sabe quién es.
    | FetchPagos ULID (Maybe ULID)
    | InvalidatePagos ULID
    | PagoFetched ULID (WebData Pago)
    | FetchPago ULID ULID
    | RepartijaFetched ULID (WebData RepartijaForFrontend)
    | FetchRepartija ULID
