module Models.Store.Types exposing (..)

import Dict exposing (Dict)
import Generated.Api exposing (Grupo, Repartija, ResumenGrupo, ShallowRepartija, ULID)
import RemoteData exposing (WebData)


type alias Store =
    { grupos : Dict ULID (WebData Grupo)
    , resumenes : Dict ULID (WebData ResumenGrupo)
    , repartijasPorGrupo : Dict ULID (WebData (List ShallowRepartija))
    , repartijas : Dict ULID (WebData Repartija)
    }


type StoreMsg
    = GrupoFetched ULID (WebData Grupo)
    | FetchGrupo ULID
    | ResumenFetched ULID (WebData ResumenGrupo)
    | FetchResumen ULID
    | RepartijasFetched ULID (WebData (List ShallowRepartija))
    | FetchRepartijas ULID
    | RepartijaFetched ULID (WebData Repartija)
    | FetchRepartija ULID
    | StoreNoOp String
