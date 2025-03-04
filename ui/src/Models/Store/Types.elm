module Models.Store.Types exposing (..)

import Dict exposing (Dict)
import Generated.Api exposing (Grupo, Netos, Repartija, ShallowRepartija, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData Grupo)
    , netos : Dict ULID (WebData Netos)
    , repartijasPorGrupo : Dict ULID (WebData (List ShallowRepartija))
    , repartijas : Dict ULID (WebData Repartija)
    }


type StoreMsg
    = GrupoFetched ULID (WebData Grupo)
    | FetchGrupo ULID
    | NetosFetched ULID (WebData Netos)
    | FetchNetos ULID
    | RepartijasFetched ULID (WebData (List ShallowRepartija))
    | FetchRepartijas ULID
    | RepartijaFetched ULID (WebData Repartija)
    | FetchRepartija ULID
    | StoreNoOp String
