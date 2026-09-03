module Pages.Grupos.GrupoId_.Liquidaciones exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Generated.Api as Api exposing (Moneda, ShallowGrupo, Transaccion, ULID)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Models.Transaccion as Transaccion
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )


type Msg
    = SaldarTransaccion ULID
    | DesmarcarTransaccion ULID
    | TransaccionResponse String (Result Http.Error ULID)


{-| En qué estado está una transferencia del congelamiento.
-}
type Estado
    = Pendiente
    | Hecha


{-| Todas las acciones terminan igual: la transacción cambió de estado y hay que
releer el resumen, porque los netos ya no son los mismos.
-}
refrescarYAvisar : Model -> String -> ( Model, Effect Msg )
refrescarYAvisar model mensaje =
    ( model
    , Effect.batch
        -- Solo el resumen: marcar una transferencia ya no crea un pago, así que
        -- ni los pagos ni el grupo cambian.
        [ Store.refreshResumen model.grupoId
        , Toasts.pushToast Toasts.ToastSuccess mensaje
        ]
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SaldarTransaccion transaccionId ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdTransaccionesByTransaccionIdSaldar
                    model.grupoId
                    transaccionId
                    (TransaccionResponse "Se registró la transferencia")
            )

        DesmarcarTransaccion transaccionId ->
            ( model
            , Effect.sendCmd <|
                Api.deleteGrupoByIdTransaccionesByTransaccionIdSaldar
                    model.grupoId
                    transaccionId
                    (TransaccionResponse "La transferencia volvió a quedar pendiente")
            )

        TransaccionResponse mensaje (Ok _) ->
            refrescarYAvisar model mensaje

        TransaccionResponse _ (Err _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastDanger "No se pudo cambiar el estado de la transferencia"
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading...", body = [] }

        Loading ->
            { title = "Cargando", body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ] }

        Failure _ ->
            { title = "Fallo", body = [] }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ viewContent store model grupo ]
                ]
            }


viewContent : Store -> Model -> ShallowGrupo -> Html Msg
viewContent store model grupo =
    case store |> Store.getResumen model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los datos del grupo." ]

        -- Sin congelar todavía no hay nada que liquidar: las transferencias
        -- existen como filas recién cuando congelar decide cuáles son.
        Success (Api.GrupoAbierto _) ->
            Bs.alert Bs.AlertInfo
                []
                [ text "Este grupo no está congelado, así que todavía no hay transferencias que hacer." ]

        Success (Api.GrupoCongelado resumen) ->
            let
                todas : List ( Estado, ( Moneda, Transaccion ) )
                todas =
                    (aplanar resumen.transaccionesParaSaldar |> List.map (Tuple.pair Pendiente))
                        ++ (aplanar resumen.transaccionesHechas |> List.map (Tuple.pair Hecha))
                        -- El id es el orden en que las creó el congelamiento, y
                        -- no cambia al marcarlas: así una fila no se mueve de
                        -- lugar por debajo de quien la está tocando.
                        |> List.sortBy (\( _, ( _, t ) ) -> t.id |> Maybe.withDefault "")
            in
            if List.isEmpty todas then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Este congelamiento no dejó transferencias." ]

            else
                div []
                    [ viewResumenDeEstados todas
                    , div [ class "list-group" ]
                        (todas |> List.map (viewTransferencia grupo))
                    ]


{-| 'PorMoneda' agrupa por moneda, pero acá las filas van todas juntas y en
orden, así que conviene la lista plana con la moneda pegada a cada una.
-}
aplanar : Api.PorMoneda (List Transaccion) -> List ( Moneda, Transaccion )
aplanar =
    List.concatMap (\( moneda, ts ) -> ts |> List.map (\t -> ( moneda, t )))


viewResumenDeEstados : List ( Estado, ( Moneda, Transaccion ) ) -> Html Msg
viewResumenDeEstados todas =
    let
        hechas =
            todas |> List.filter (\( estado, _ ) -> estado == Hecha) |> List.length
    in
    div [ class "text-muted small mb-3" ]
        [ text (String.fromInt hechas)
        , text " de "
        , text (String.fromInt (List.length todas))
        , text
            (if List.length todas == 1 then
                " transferencia hecha"

             else
                " transferencias hechas"
            )
        ]


{-| Una fila de la lista: el estado, quién le transfiere a quién, cuánto, y la
acción que lo cambia. Sin distinguir entre "tuyas" y "ajenas": esta pantalla es
para ver y corregir el congelamiento entero.
-}
viewTransferencia : ShallowGrupo -> ( Estado, ( Moneda, Transaccion ) ) -> Html Msg
viewTransferencia grupo ( estado, ( moneda, t ) ) =
    div [ class "list-group-item d-flex align-items-center gap-3 flex-wrap" ]
        [ case estado of
            Pendiente ->
                span [ class "badge text-bg-secondary" ] [ text "Pendiente" ]

            Hecha ->
                span [ class "badge text-bg-success" ] [ text "Hecha" ]
        , span
            [ class "flex-grow-1"
            , class
                (case estado of
                    Pendiente ->
                        ""

                    Hecha ->
                        "text-muted"
                )
            ]
            (Transaccion.frase grupo moneda t)
        , case ( estado, t.id ) of
            ( Pendiente, Just transaccionId ) ->
                Bs.btn Bs.Secondary
                    [ class "btn-sm text-nowrap"
                    , onClick (SaldarTransaccion transaccionId)
                    ]
                    [ text "Marcar como hecha" ]

            ( Hecha, Just transaccionId ) ->
                Bs.btn Bs.Secondary
                    [ class "btn-sm text-nowrap"
                    , onClick (DesmarcarTransaccion transaccionId)
                    ]
                    [ text "Volver a pendiente" ]

            ( _, Nothing ) ->
                text ""
        ]
