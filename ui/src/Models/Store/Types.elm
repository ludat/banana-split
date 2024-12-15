module Models.Store.Types exposing (..)

import Dict exposing (Dict)
import Generated.Api exposing (Grupo, Netos, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData Grupo)
    , netos : Dict ULID (WebData Netos)
    }


type StoreMsg
    = GrupoFetched ULID (WebData Grupo)
    | FetchGrupo ULID
    | NetosFetched ULID (WebData Netos)
    | FetchNetos ULID
    | StoreNoOp String
