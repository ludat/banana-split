module Layouts.Default.Grupo exposing (Agregarme, ErrorAlAgregarme, Model, Msg, Props, layout)

import Browser.Dom
import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Css
import Dict
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (ClaimParticipanteResult(..), Grupo, Participante, ParticipanteAddParams, ParticipanteClaimError(..), ULID, User)
import Html exposing (Html, a, button, div, h2, i, label, li, node, option, p, select, span, text, ul)
import Html.Attributes as Attr exposing (class, classList, selected, style, type_, value)
import Html.Events exposing (on, onClick, onSubmit, preventDefaultOn)
import Http
import Json.Decode as Decode
import Layout exposing (Layout)
import Layouts.Default
import Models.Grupo exposing (GrupoLike, claimErrorMessage, currentParticipante, estaCongelado, grupoIdFromPath, ownedParticipante, participanteErrorFromHttp)
import Models.Repartija as Repartija
import Models.Store as Store
import Models.Store.Types exposing (Store)
import QRCode
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Svg.Attributes as SvgAttr
import Task
import Utils.Form exposing (CustomFormError, anyString, isDataModifyingEvent, textInput)
import Utils.Telemetry as Telemetry
import Utils.Toasts.Types exposing (ToastLevel(..))
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout Layouts.Default.Props Model Msg contentMsg
layout _ shared route =
    Layout.new
        { init = \() -> init route
        , update = update (PagoDetalleModal.context shared route) shared.store shared.currentUser
        , view = view shared.store route.path shared.participanteId shared.origin shared.currentUser
        , subscriptions = subscriptions
        }
        |> Layout.withParentProps {}
        |> Layout.withOnUrlChanged (PagoModalMsg << PagoDetalleModal.onUrlChanged)



-- MODEL


type alias Model =
    { qrShare : Maybe { title : String, url : String }
    , pagoModal : PagoDetalleModal.Model
    , agregarme : Maybe Agregarme
    }


{-| El modal de "Soy nuevo": alguien que entró al grupo y no es ninguno de los
participantes se agrega a sí mismo. Con cuenta queda reclamado de una; sin
cuenta es un participante más, elegido en este navegador. `envio` es el último
intento de agregarse.
-}
type alias Agregarme =
    { grupoId : ULID
    , form : Form CustomFormError ParticipanteAddParams
    , envio : RemoteData ErrorAlAgregarme Participante
    }


{-| Por qué no se pudo agregar. El rechazo es del backend, que es el único que
valida el nombre: con cuenta llega tipado, y sin cuenta como un status que se
traduce al mismo rechazo.
-}
type ErrorAlAgregarme
    = Rechazado ParticipanteClaimError
    | FalloElRequest


validateAgregarme : Validation CustomFormError ParticipanteAddParams
validateAgregarme =
    V.succeed ParticipanteAddParams
        |> V.andMap (V.field "nombre" anyString)


init : Route () -> ( Model, Effect Msg )
init route =
    let
        ( pagoModal, modalEffect ) =
            PagoDetalleModal.init route
    in
    ( { qrShare = Nothing
      , pagoModal = pagoModal
      , agregarme = Nothing
      }
    , Effect.map PagoModalMsg modalEffect
    )



-- UPDATE


type Msg
    = ForwardSharedMessage Shared.Msg
    | ShareUrl { title : String, url : String }
    | OpenQrShare { title : String, url : String }
    | CloseQrShare
    | PagoModalMsg PagoDetalleModal.Msg
    | AbrirPopup PagoDetalleModal.Ruta
    | AbrirAgregarme ULID
    | CerrarAgregarme
    | AgregarmeForm Form.Msg
    | GotAgregarme (Result ErrorAlAgregarme Participante)
    | NoOp


update : PagoDetalleModal.Context -> Store -> WebData User -> Msg -> Model -> ( Model, Effect Msg )
update ctx store currentUser msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        AbrirAgregarme grupoId ->
            ( { model
                | agregarme =
                    Just
                        { grupoId = grupoId
                        , form =
                            Form.initial
                                [ Form.setString "nombre" <|
                                    case currentUser of
                                        Success u ->
                                            u.nombre

                                        _ ->
                                            ""
                                ]
                                validateAgregarme
                        , envio = NotAsked
                        }
              }
            , Effect.sendCmd <| Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "agregarme-nombre"
            )

        CerrarAgregarme ->
            ( { model | agregarme = Nothing }, Effect.none )

        AgregarmeForm Form.Submit ->
            case model.agregarme of
                Just agregarme ->
                    enviarAgregarme currentUser agregarme model

                Nothing ->
                    ( model, Effect.none )

        AgregarmeForm formMsg ->
            ( { model
                | agregarme =
                    model.agregarme
                        |> Maybe.map
                            (\a ->
                                { a
                                    | form = Form.update validateAgregarme formMsg a.form
                                    , envio =
                                        -- El rechazo era de lo que estaba escrito: si cambia, ya no aplica.
                                        if isDataModifyingEvent formMsg then
                                            NotAsked

                                        else
                                            a.envio
                                }
                            )
              }
            , Effect.none
            )

        GotAgregarme result ->
            case ( model.agregarme, result ) of
                ( Just agregarme, Ok p ) ->
                    ( { model | agregarme = Nothing }
                    , Effect.batch
                        [ Store.refreshGrupo agregarme.grupoId
                        , Effect.sendSharedMsg <|
                            Shared.SetCurrentParticipante { grupoId = agregarme.grupoId, participanteId = Just p.id }
                        , Effect.sendToast { level = ToastSuccess, content = "Te agregamos al grupo como " ++ p.nombre }
                        ]
                    )

                ( Just agregarme, Err error ) ->
                    ( { model | agregarme = Just { agregarme | envio = Failure error } }
                    , Effect.none
                    )

                ( Nothing, _ ) ->
                    ( model, Effect.none )

        PagoModalMsg subMsg ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.update ctx store subMsg model.pagoModal
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )

        AbrirPopup ruta ->
            ( model, Effect.replaceRoute ruta )

        ForwardSharedMessage sharedMsg ->
            ( model
            , Effect.sendSharedMsg sharedMsg
            )

        ShareUrl data ->
            ( model
            , Effect.share data
            )

        OpenQrShare data ->
            ( { model | qrShare = Just data }
            , Effect.none
            )

        CloseQrShare ->
            ( { model | qrShare = Nothing }
            , Effect.none
            )


enviarAgregarme : WebData User -> Agregarme -> Model -> ( Model, Effect Msg )
enviarAgregarme currentUser agregarme model =
    let
        enviado =
            { agregarme | form = Form.update validateAgregarme Form.Submit agregarme.form }
    in
    case ( RemoteData.isLoading agregarme.envio, Form.getOutput agregarme.form ) of
        ( False, Just params ) ->
            ( { model | agregarme = Just { enviado | envio = Loading } }
            , Effect.sendCmd <|
                case currentUser of
                    -- Con cuenta queda reclamado en el mismo paso.
                    Success _ ->
                        Api.postGrupoByIdParticipantesMe agregarme.grupoId
                            params
                            (\result ->
                                GotAgregarme <|
                                    case result of
                                        Ok (ClaimAccepted p) ->
                                            Ok p

                                        Ok (ClaimRejected reason) ->
                                            Err (Rechazado reason)

                                        Err _ ->
                                            Err FalloElRequest
                            )

                    _ ->
                        Api.postGrupoByIdParticipantes agregarme.grupoId
                            params
                            (GotAgregarme << Result.mapError errorAlAgregarmeSinCuenta)
            )

        _ ->
            ( { model | agregarme = Just enviado }, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Store
    -> Path.Path
    -> Maybe ULID
    -> String
    -> WebData User
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view store currentPath manualPick origin currentUser { toContentMsg, model, content } =
    let
        remoteGrupo =
            grupoIdFromPath currentPath
                |> Maybe.map (\grupoId -> Store.getGrupo grupoId store)
                |> Maybe.withDefault NotAsked

        -- The participante to act as: the manual pick, or the participante the
        -- logged-in account owns in this grupo (derived, never persisted).
        activeUser =
            case remoteGrupo of
                Success grupo ->
                    currentParticipante manualPick currentUser grupo

                _ ->
                    manualPick
    in
    { title = content.title
    , body =
        [ case remoteGrupo of
            Success grupo ->
                node "pwa-manifest"
                    [ Attr.attribute "grupo-id" grupo.id
                    , Attr.attribute "nombre" grupo.nombre
                    ]
                    []

            _ ->
                text ""
        , case remoteGrupo of
            Success grupo ->
                Html.map toContentMsg <|
                    viewGroupHeader origin currentPath activeUser currentUser store grupo

            _ ->
                text ""
        , Html.map toContentMsg <|
            viewQrModal model.qrShare
        , case remoteGrupo of
            Success grupo ->
                Html.map (toContentMsg << PagoModalMsg) <|
                    PagoDetalleModal.view store grupo model.pagoModal

            _ ->
                text ""
        ]
            ++ (case remoteGrupo of
                    Success grupo ->
                        if eligiendoParticipante activeUser grupo then
                            [ Html.map toContentMsg <|
                                div
                                    [ Attr.id "grupo-elegir-participante"
                                    , class "container-fluid py-5 text-center d-flex flex-column align-items-center justify-content-center"
                                    , style "min-height" "60vh"
                                    ]
                                    [ Html.h4 [ class "fw-semibold mb-2" ] [ text "Bienvenido" ]
                                    , p [ class "mb-3" ] [ text "Seleccioná quién sos para comenzar" ]
                                    , viewSelectorDeBienvenida activeUser grupo

                                    -- Todavía no eligió a nadie, así que con
                                    -- cuenta tampoco tiene a nadie reclamado.
                                    , p [ class "mt-4 mb-1 text-muted" ] [ text "¿Aún no estás en el grupo?" ]

                                    -- Como los chips de la repartija: un
                                    -- dropdown que Bootstrap abre, cierra
                                    -- (clic afuera, Escape) y Popper ubica.
                                    -- No se cierra al tocar lo de adentro
                                    -- (`data-bs-auto-close`), que es un form.
                                    , div [ class "dropdown-center" ]
                                        [ button
                                            [ type_ "button"
                                            , class "btn btn-link p-0 text-decoration-none d-inline-flex align-items-center gap-1"
                                            , Attr.attribute "data-bs-toggle" "dropdown"
                                            , Attr.attribute "data-bs-auto-close" "outside"
                                            , Attr.attribute "data-bs-offset" "0,8"
                                            , Attr.attribute "aria-expanded" "false"
                                            , Telemetry.click "grupo.agregarme_abrir"

                                            -- Cerrándolo con este mismo botón, el
                                            -- click llega antes que el cierre: ahí
                                            -- no hay que re-armar el form.
                                            , onClick
                                                (if model.agregarme == Nothing then
                                                    AbrirAgregarme grupo.id

                                                 else
                                                    NoOp
                                                )

                                            -- Bootstrap avisa en el botón. Mientras
                                            -- se envía no se cierra: la respuesta
                                            -- es la que lo elige. Cerrado, se
                                            -- descarta lo escrito, como el modal.
                                            , preventDefaultOn "hide.bs.dropdown"
                                                (Decode.succeed
                                                    ( NoOp
                                                    , model.agregarme
                                                        |> Maybe.map (.envio >> RemoteData.isLoading)
                                                        |> Maybe.withDefault False
                                                    )
                                                )
                                            , on "hidden.bs.dropdown" (Decode.succeed CerrarAgregarme)
                                            ]
                                            [ i [ class "bi bi-plus-circle-fill" ] []
                                            , text "Agregarme a la lista"
                                            ]
                                        , viewAgregarmePopover model.agregarme
                                        ]
                                    ]
                            ]

                        else
                            content.body

                    _ ->
                        content.body
               )
            ++ [ case remoteGrupo of
                    Success grupo ->
                        if (headerInfo currentPath store currentUser grupo).showTabs && not (eligiendoParticipante activeUser grupo) then
                            div
                                [ class "d-md-none"
                                , style "height" "calc(var(--navbar-bottom-height) + 1.55rem + env(safe-area-inset-bottom))"
                                ]
                                []

                        else
                            text ""

                    _ ->
                        text ""
               ]
    }


viewGroupHeader : String -> Path.Path -> Maybe ULID -> WebData User -> Store -> Grupo -> Html Msg
viewGroupHeader origin currentPath activeUser currentUser store grupo =
    let
        info =
            headerInfo currentPath store currentUser grupo

        share =
            { title = info.share.title
            , url = origin ++ Path.toString info.share.path
            }
    in
    if eligiendoParticipante activeUser grupo then
        viewGroupHeaderDeBienvenida activeUser info share grupo

    else
        viewGroupHeaderCompleto currentPath activeUser currentUser store grupo info share


{-| Mientras no eligió quién es, todavía no hay nada que hacer en el grupo: ni
tabs, ni "Agregar gasto", ni la navbar de abajo. Queda el nombre del grupo, para
compartirlo, y el "Ver cómo" en desktop (en mobile el selector ya está en el
medio de la pantalla).
-}
viewGroupHeaderDeBienvenida : Maybe ULID -> HeaderInfo -> { title : String, url : String } -> Grupo -> Html Msg
viewGroupHeaderDeBienvenida activeUser info share grupo =
    div [ Attr.id "grupo-header", class "border-bottom" ]
        [ div [ class "container-fluid py-3" ]
            [ viewBreadcrumb info.breadcrumb
            , div [ class "d-flex flex-wrap align-items-center justify-content-between gap-3" ]
                [ div [ class "d-flex align-items-center gap-3 min-w-0" ]
                    [ h2 [ class "mb-0 display-6 text-break" ] [ text info.title ]
                    , viewShareDropdown
                        [ i [ class "bi bi-share-fill" ] []
                        , span [ class "visually-hidden" ] [ text "Compartir" ]
                        ]
                        share
                    ]
                , label [ class "d-none d-md-flex align-items-center gap-2 text-nowrap mb-0" ]
                    [ text "Ver cómo:"
                    , viewGlobalUserSelector activeUser grupo
                    ]
                ]
            ]
        ]


viewGroupHeaderCompleto : Path.Path -> Maybe ULID -> WebData User -> Store -> Grupo -> HeaderInfo -> { title : String, url : String } -> Html Msg
viewGroupHeaderCompleto currentPath activeUser currentUser store grupo info share =
    div [ Attr.id "grupo-header", class "border-bottom" ]
        [ div [ class "container-fluid py-3" ]
            [ div [ class "d-flex flex-wrap align-items-start justify-content-between gap-3" ]
                [ div []
                    [ viewBreadcrumb info.breadcrumb
                    , h2 [ class "mb-0 fw-bold d-flex align-items-center gap-2" ]
                        (viewIconoDelTitulo currentPath
                            :: text info.title
                            :: viewAlertaDelTitulo currentPath store
                        )
                    , viewBadgeCongelado grupo
                    ]

                -- Solo desktop. En mobile el "Ver como" baja a su propia banda
                -- y las acciones se van a la navbar de abajo: compartir al menú
                -- "Más", y agregar gasto al botón del medio.
                , div [ Attr.id "grupo-header-acciones", class "d-none d-md-flex flex-column align-items-end gap-2" ]
                    [ label [ class "d-flex align-items-center gap-2 small text-muted text-nowrap" ]
                        [ text "Ver como:"
                        , viewGlobalUserSelector activeUser grupo
                        ]
                    , viewVerComoWarning currentUser activeUser grupo
                    , div [ class "d-flex flex-wrap align-items-center gap-2" ]
                        [ viewShareDropdown textoCompartir share

                        -- Fuera de las secciones con tabs (la repartija) se
                        -- está mirando un gasto puntual, no cargando gastos.
                        , if info.showTabs then
                            a
                                (class "btn btn-primary d-inline-flex align-items-center"
                                    :: Telemetry.click "grupo.new_gasto"
                                    :: PagoDetalleModal.linkANuevoGasto AbrirPopup currentPath
                                )
                                [ i [ class "bi bi-plus-lg me-1" ] []
                                , text "Agregar gasto"
                                ]

                          else
                            text ""
                        ]
                    ]
                ]
            ]

        -- La banda de "Ver como" de mobile: separada del título por su propio
        -- borde, como en el diseño.
        , div [ Attr.id "grupo-header-mobile", class "d-md-none border-top" ]
            [ div [ class "container-fluid py-2 d-flex align-items-center flex-wrap gap-2" ]
                [ label [ class "d-flex align-items-center gap-2 small text-muted text-nowrap mb-0" ]
                    [ text "Ver como:"
                    , viewGlobalUserSelector activeUser grupo
                    ]
                , viewVerComoWarning currentUser activeUser grupo

                -- Compartir vive en el menú "Más" de la navbar inferior, pero
                -- esa navbar solo existe en las secciones con tabs. Donde no
                -- está —la repartija, que es justamente la que más se
                -- comparte— el botón se queda acá para no perderlo.
                , if info.showTabs then
                    text ""

                  else
                    div [ class "ms-auto" ] [ viewShareDropdown textoCompartir share ]
                ]
            ]
        , if info.showTabs then
            div [ Attr.id "grupo-tabs", class "container-fluid d-none d-md-block" ]
                [ viewTabNav currentPath grupo ]

          else
            text ""
        , if info.showTabs then
            viewBottomNav currentPath share grupo

          else
            text ""
        ]


viewIconoDelTitulo : Path.Path -> Html Msg
viewIconoDelTitulo currentPath =
    case currentPath of
        Path.Grupos_GrupoId__Repartijas_RepartijaId_ _ ->
            span
                [ class "d-inline-flex align-items-center justify-content-center rounded-circle bg-body-secondary fs-5 flex-shrink-0"
                , style "width" "2.5rem"
                , style "height" "2.5rem"
                , Attr.attribute "aria-hidden" "true"
                ]
                [ i [ class "bi bi-people-fill" ] [] ]

        _ ->
            text ""


{-| La alerta al lado del título de la repartija cuando todavía hay items que
no están bien repartidos.
-}
viewAlertaDelTitulo : Path.Path -> Store -> List (Html Msg)
viewAlertaDelTitulo currentPath store =
    case currentPath of
        Path.Grupos_GrupoId__Repartijas_RepartijaId_ params ->
            case Store.getRepartija params.repartijaId store of
                Success repartijaPage ->
                    if Repartija.hayItemsNoResueltos repartijaPage.detalle then
                        [ i
                            [ class "bi bi-exclamation-triangle-fill text-danger fs-4"
                            , Attr.title "Aún hay items no resueltos"
                            , Attr.attribute "aria-label" "Aún hay items no resueltos"
                            ]
                            []
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


viewBadgeCongelado : Grupo -> Html Msg
viewBadgeCongelado grupo =
    if estaCongelado grupo then
        div [ class "mt-2" ]
            [ Bs.badge "d-inline-flex align-items-center gap-1 text-uppercase text-white"
                [ Bs.fondoCongelado ]
                [ i [ class "bi bi-cash-stack" ] []
                , text "Saldando deudas"
                ]
            ]

    else
        text ""


{-| Inline warning shown next to the "Ver como" toggle when a logged-in user is
acting as a participante that isn't theirs. If that participante is unclaimed we
offer a quick "reclamar"; if it's taken by someone else we just warn.
-}
viewVerComoWarning : WebData User -> Maybe ULID -> Grupo -> Html Msg
viewVerComoWarning currentUser activeUser grupo =
    case ( currentUser, activeUser ) of
        ( Success u, Just uid ) ->
            case grupo.participantes |> List.filter (\p -> p.id == uid) |> List.head of
                Just p ->
                    if (p.user |> Maybe.map .id) == Just u.id then
                        text ""

                    else
                        let
                            -- Only offer a quick "reclamar" when the participante
                            -- is unclaimed AND the user doesn't already own one in
                            -- the grupo (a user owns at most one).
                            canClaim =
                                p.user == Nothing && ownedParticipante u.id grupo == Nothing
                        in
                        div [ class "d-flex align-items-center gap-1 text-warning small" ]
                            (i [ class "bi bi-exclamation-triangle" ] []
                                :: text "No sos vos"
                                :: (if canClaim then
                                        [ button
                                            [ type_ "button"
                                            , class "btn btn-sm btn-link p-0 text-decoration-none align-baseline"
                                            , Telemetry.click "grupo.claim_from_warning"
                                            , onClick
                                                (ForwardSharedMessage <|
                                                    Shared.ClaimParticipante { grupoId = grupo.id, participanteId = p.id }
                                                )
                                            ]
                                            [ text "reclamar" ]
                                        ]

                                    else
                                        []
                                   )
                            )

                Nothing ->
                    text ""

        _ ->
            text ""


{-| Adónde te devuelve el "‹". Es uno solo: no un camino completo desde la raíz,
sino un salto para atrás. En casi todas las secciones es el grupo, en la
repartija es el gasto, y en el resumen del grupo es la lista de grupos.

El `label` de una entidad lleva el tipo adelante —"Grupo: Salidita de Jueves"—
para que no haya que adivinar qué es ese nombre.

Lleva una ruta entera y no un path pelado porque la del gasto apunta al popup,
que vive en la query.

-}
type alias Crumb =
    { label : String, ruta : PagoDetalleModal.Ruta }


{-| Qué te ofrece el breadcrumb. Casi siempre volver a la entidad de la que
colgás, pero en el resumen de un grupo depende de si hay sesión: sin sesión no
hay grupos que listar, así que mandar a alguien a "Tus grupos" lo deja mirando
una página vacía, y en ese caso ofrece tener una cuenta.

Va envuelto en un `Maybe` donde `Nothing` es "todavía no sé cuál de las dos" —
falta el gasto de la repartija, o falta saber si hay sesión—. Eso no es una
acción, así que no entra como un caso más acá adentro.

-}
type AccionDelBreadcrumb
    = Volver (List Crumb)
    | InvitarASesion PagoDetalleModal.Ruta


sinQuery : Path.Path -> PagoDetalleModal.Ruta
sinQuery path =
    { path = path, query = Dict.empty, hash = Nothing }


{-| El login con el `redirect` puesto, para volver a donde estabas. Es el mismo
parámetro que usa el "Iniciar sesión" de la navbar.
-}
rutaDeLogin : Path.Path -> PagoDetalleModal.Ruta
rutaDeLogin volverA =
    { path = Path.Login
    , query = Dict.singleton "redirect" (Path.toString volverA)
    , hash = Nothing
    }


{-| Computes everything the group header needs from the current path and store:
de qué entidad colgás, the big `h2` title, and whether the section tabs should
be shown (only on top-level sections).

Entity names are resolved from the store, falling back to `"Cargando..."` while the
data is still loading.

-}
type alias HeaderInfo =
    { breadcrumb : Maybe AccionDelBreadcrumb, title : String, showTabs : Bool, share : { title : String, path : Path.Path } }


headerInfo : Path.Path -> Store -> WebData User -> Grupo -> HeaderInfo
headerInfo currentPath store currentUser grupo =
    let
        -- De acá cuelgan todas las secciones del grupo. La única que no es
        -- esta es la repartija, que cuelga de su gasto.
        volverAlGrupo : Maybe AccionDelBreadcrumb
        volverAlGrupo =
            Just <|
                Volver
                    [ { label = "Grupo: " ++ grupo.nombre
                      , ruta = sinQuery (Path.Grupos_Id_ { id = grupo.id })
                      }
                    ]

        grupoShare =
            { title = grupo.nombre, path = Path.Grupos_Id_ { id = grupo.id } }
    in
    case currentPath of
        Path.Grupos_GrupoId__Gastos _ ->
            { breadcrumb = volverAlGrupo
            , title = "Gastos"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Transferencias _ ->
            { breadcrumb = volverAlGrupo
            , title = "Transferencias"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Liquidaciones _ ->
            { breadcrumb = volverAlGrupo
            , title = "Transferencias"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Participantes _ ->
            { breadcrumb = volverAlGrupo
            , title = "Participantes"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Settings _ ->
            { breadcrumb = volverAlGrupo
            , title = "Ajustes"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Repartijas_RepartijaId_ params ->
            let
                maybeRepartija =
                    Store.getRepartija params.repartijaId store
                        |> RemoteData.toMaybe

                pagoNombre =
                    maybeRepartija
                        |> Maybe.map .pagoNombre
                        |> Maybe.withDefault "Cargando"
            in
            { -- La repartija es lo único que no cuelga del grupo: cuelga del
              -- gasto que la generó, y ahí es adonde tenés que poder volver.
              -- Mientras el gasto no cargó todavía no hay a dónde apuntar.
              breadcrumb =
                maybeRepartija
                    |> Maybe.map
                        (\r ->
                            Volver
                                [ { label = "Gastos:"
                                  , ruta = sinQuery (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
                                  }
                                , { label = pagoNombre
                                  , ruta =
                                        PagoDetalleModal.rutaGasto
                                            (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
                                            r.pagoId
                                  }
                                ]
                        )
            , title = "Repartija"
            , showTabs = False
            , share = { title = "Deudores de " ++ pagoNombre, path = Path.Grupos_GrupoId__Repartijas_RepartijaId_ params }
            }

        -- Rutas viejas: redirigen apenas se montan, así que nunca llegan a
        -- pintar chrome. Están acá solo para que el case sea exhaustivo.
        Path.Grupos_GrupoId__Gastos_New _ ->
            { breadcrumb = volverAlGrupo
            , title = "Nuevo gasto"
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Gastos_GastoId_ _ ->
            { breadcrumb = volverAlGrupo
            , title = "Cargando..."
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos _ ->
            { breadcrumb = volverAlGrupo
            , title = "Gastos"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos_New _ ->
            { breadcrumb = volverAlGrupo
            , title = "Nuevo gasto"
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos_PagoId_ _ ->
            { breadcrumb = volverAlGrupo
            , title = "Cargando..."
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        -- El grupo no cuelga de otra entidad, cuelga de tu lista de grupos. Y
        -- el título ya es el nombre del grupo, así que repetirlo acá arriba no
        -- diría nada nuevo.
        --
        -- Sin sesión esa lista está vacía, así que en vez del link va la
        -- invitación a tener una cuenta. Y mientras el pedido está en vuelo
        -- todavía no se sabe cuál de las dos corresponde.
        Path.Grupos_Id_ _ ->
            { breadcrumb =
                case currentUser of
                    Success _ ->
                        Just <| Volver [ { label = "Tus grupos", ruta = sinQuery Path.Home_ } ]

                    Failure _ ->
                        Just <| InvitarASesion (rutaDeLogin currentPath)

                    NotAsked ->
                        Just <| InvitarASesion (rutaDeLogin currentPath)

                    Loading ->
                        Nothing
            , title = grupo.nombre
            , showTabs = True
            , share = grupoShare
            }

        Path.NotFound_ ->
            { breadcrumb = Nothing
            , title = grupo.nombre
            , showTabs = False
            , share = grupoShare
            }

        Path.Home_ ->
            { breadcrumb = Nothing
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }

        Path.Login ->
            { breadcrumb = Nothing
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }

        Path.Cuenta ->
            { breadcrumb = Nothing
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }


{-| La vuelta a la entidad de la que colgás, con forma de "‹ Grupo: Salidita de
Jueves". No es un camino desde la raíz sino un solo salto para atrás; cuando
tiene más de un crumb (la sección y la entidad, "‹ Gastos: Cena") cada parte es
su propio link.

La invitación a tener cuenta ocupa el mismo lugar pero no lleva el "‹": no te
devuelve a ningún lado, te ofrece algo.

-}
viewBreadcrumb : Maybe AccionDelBreadcrumb -> Html Msg
viewBreadcrumb accion =
    case accion of
        -- Mientras no se sabe qué ofrecer no va nada: un placeholder acá
        -- movería el título para abajo y después lo acomodaría, que molesta
        -- más que esperar.
        Nothing ->
            text ""

        -- Cada crumb es su propio link: en "‹ Gastos: Cena" la sección lleva a
        -- la lista y el nombre a ese gasto dentro de ella. El "‹" va una sola
        -- vez, adentro del primer link.
        Just (Volver crumbs) ->
            Html.nav [ Attr.id "grupo-breadcrumb", Attr.attribute "aria-label" "breadcrumb", class "mb-1 d-flex align-items-center gap-1" ]
                (List.indexedMap
                    (\indice crumb ->
                        Html.a
                            [ Route.href crumb.ruta
                            , class "text-decoration-none d-inline-flex align-items-center gap-1"
                            , Telemetry.click "grupo.breadcrumb"
                            ]
                            (if indice == 0 then
                                [ Html.span [ Attr.attribute "aria-hidden" "true" ] [ text "‹" ]
                                , Html.span [] [ text crumb.label ]
                                ]

                             else
                                [ text crumb.label ]
                            )
                    )
                    crumbs
                )

        Just (InvitarASesion rutaLogin) ->
            div [ Attr.id "grupo-breadcrumb", class "mb-1" ]
                [ Html.a
                    [ Route.href rutaLogin
                    , class "text-decoration-none d-inline-flex align-items-center gap-1 small"
                    , Telemetry.click "grupo.breadcrumb_login"
                    , Attr.title "Con una cuenta ves todos los grupos en los que participás, sin tener que acordarte las URLs."
                    ]
                    [ i [ class "bi bi-person-circle" ] []
                    , Html.span [] [ text "Iniciá sesión para ver tus grupos" ]
                    ]
                ]


viewTabNav : Path.Path -> Grupo -> Html Msg
viewTabNav currentPath grupo =
    Bs.navTabs [ class "border-0 mb-0" ]
        [ Bs.navTab
            { active = currentPath == Path.Grupos_Id_ { id = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_Id_ { id = grupo.id }, Telemetry.click "nav_tabs.resumen" ]
            }
            [ text "Resumen" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }, Telemetry.click "nav_tabs.gastos" ]
            }
            [ text "Gastos" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id }, Telemetry.click "nav_tabs.transferencias" ]
            }
            [ text "Transferencias" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.id }, Telemetry.click "nav_tabs.participantes" ]
            }
            [ text "Participantes" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Settings { grupoId = grupo.id }, Telemetry.click "nav_tabs.settings" ]
            }
            [ text "Ajustes" ]
        ]


{-| The mobile-only bottom tab bar. Mirrors the desktop top tab nav plus a
raised "Ingresar pago" FAB in the centre and a "Más" popover (opening upward)
for the sections that don't fit on the bar. Hidden on `md+` via the
`navbar-bottom` component's own media query.

El menú "Más" también es donde vive compartir en mobile: en el header solo está
de `md` para arriba.

-}
viewBottomNav :
    Path.Path
    -> { title : String, url : String }
    -> Grupo
    -> Html Msg
viewBottomNav currentPath share grupo =
    let
        item track icon label path =
            a
                [ classList [ ( "navbar-item", True ), ( "active", currentPath == path ) ]
                , Path.href path
                , Telemetry.click track
                ]
                [ i [ class ("bi " ++ icon) ] []
                , Html.span [ class "text-truncate" ] [ text label ]
                ]

        masActive =
            (currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id })
                || (currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id })
    in
    Html.nav [ Attr.id "grupo-nav-inferior", Css.navbar_bottom, Css.barra_inferior_fija ]
        [ item "nav_bottom.resumen" "bi-house-door" "Resumen" (Path.Grupos_Id_ { id = grupo.id })
        , item "nav_bottom.gastos" "bi-card-list" "Gastos" (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
        , a
            (Css.navbar_item
                :: Telemetry.click "nav_bottom.new_gasto"
                :: PagoDetalleModal.linkANuevoGasto AbrirPopup currentPath
            )
            [ Html.span [ Css.navbar_big_button ]
                [ i [ class "bi bi-plus-lg" ] [] ]
            , Html.span [] [ text "Ingresar gasto" ]
            ]
        , item "nav_bottom.transferencias" "bi-wallet2" "Transferencias" (Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id })
        , div [ Css.navbar_more, class "dropup" ]
            [ a
                [ classList [ ( "navbar-item", True ), ( "active", masActive ) ]
                , Attr.attribute "role" "button"
                , Attr.attribute "data-bs-toggle" "dropdown"
                , Attr.attribute "aria-expanded" "false"
                ]
                [ i [ class "bi bi-three-dots" ] []
                , Html.span [] [ text "Más" ]
                ]
            , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
                [ li []
                    [ a
                        [ class "dropdown-item"
                        , classList [ ( "active", currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id } ) ]
                        , Path.href (Path.Grupos_GrupoId__Participantes { grupoId = grupo.id })
                        , Telemetry.click "nav_bottom.participantes"
                        ]
                        [ i [ class "bi bi-people me-2" ] []
                        , text "Participantes"
                        ]
                    ]
                , li []
                    [ a
                        [ class "dropdown-item"
                        , classList [ ( "active", currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id } ) ]
                        , Path.href (Path.Grupos_GrupoId__Settings { grupoId = grupo.id })
                        , Telemetry.click "nav_bottom.settings"
                        ]
                        [ i [ class "bi bi-gear me-2" ] []
                        , text "Ajustes"
                        ]
                    ]
                , li [] [ Html.hr [ class "dropdown-divider" ] [] ]
                , li []
                    [ a
                        [ class "dropdown-item"
                        , Attr.href "#"
                        , Telemetry.click "nav_bottom.share_link"
                        , preventDefaultOn "click"
                            (Decode.succeed ( ShareUrl share, True ))
                        ]
                        [ i [ class "bi bi-link-45deg me-2" ] []
                        , text "Compartir link"
                        ]
                    ]
                , li []
                    [ a
                        [ class "dropdown-item"
                        , Attr.href "#"
                        , Telemetry.click "nav_bottom.share_qr"
                        , preventDefaultOn "click"
                            (Decode.succeed ( OpenQrShare share, True ))
                        ]
                        [ i [ class "bi bi-qr-code me-2" ] []
                        , text "Código QR"
                        ]
                    ]
                ]
            ]
        ]


{-| The "Compartir" split button: a native/clipboard share plus a QR code
option, both pointing at the current page's share target. Opening and closing
(including closing on blur) is handled by Bootstrap's own dropdown JS via
`data-bs-toggle`.
-}
viewShareDropdown : List (Html Msg) -> { title : String, url : String } -> Html Msg
viewShareDropdown contenido { title, url } =
    div [ class "dropdown" ]
        [ button
            [ type_ "button"
            , class "btn btn-outline-secondary dropdown-toggle"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            ]
            contenido
        , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
            [ li []
                [ a
                    [ class "dropdown-item"
                    , Attr.href "#"
                    , Telemetry.click "grupo.share_link"
                    , preventDefaultOn "click"
                        (Decode.succeed ( ShareUrl { title = title, url = url }, True ))
                    ]
                    [ i [ class "bi bi-link-45deg me-2" ] []
                    , text "Compartir link"
                    ]
                ]
            , li []
                [ a
                    [ class "dropdown-item"
                    , Attr.href "#"
                    , Telemetry.click "grupo.share_qr"
                    , preventDefaultOn "click"
                        (Decode.succeed ( OpenQrShare { title = title, url = url }, True ))
                    ]
                    [ i [ class "bi bi-qr-code me-2" ] []
                    , text "Código QR"
                    ]
                ]
            ]
        ]


{-| Modal showing a scannable QR code of the group's link.
-}
viewQrModal : Maybe { title : String, url : String } -> Html Msg
viewQrModal qrShare =
    let
        qrImage url =
            QRCode.fromString url
                |> Result.map
                    (QRCode.toSvg
                        [ SvgAttr.width "240px"
                        , SvgAttr.height "240px"
                        ]
                    )
                |> Result.withDefault (text "No se pudo generar el código QR")
    in
    Bs.modal
        { id = "modal-qr"
        , isOpen = qrShare /= Nothing
        , onClose = CloseQrShare
        , centered = False
        , title =
            qrShare
                |> Maybe.map .title
                |> Maybe.withDefault "Código QR"
        , body =
            case qrShare of
                Just { url } ->
                    [ div [ class "text-center" ]
                        [ div
                            [ class "d-inline-block bg-white rounded p-3"
                            , style "line-height" "0"
                            ]
                            [ qrImage url ]
                        , p [ class "text-muted small mt-3 mb-0 text-break" ] [ text url ]
                        ]
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "grupo.qr_close", onClick CloseQrShare ] [ text "Cerrar" ] ]
        }


{-| El popover de "Soy nuevo", con la misma pinta que el de los items que
reclamó alguien en la repartija. Con cuenta el nombre arranca con el de la
cuenta.

El menú está siempre en el DOM porque Bootstrap lo busca al lado del botón en el
momento del clic; el form aparece cuando `AbrirAgregarme` lo arma.

-}
viewAgregarmePopover : Maybe Agregarme -> Html Msg
viewAgregarmePopover agregarme =
    div
        [ Attr.id "agregarme-popover"
        , class "dropdown-menu shadow p-0 overflow-hidden text-start"
        , style "width" "20rem"
        , style "max-width" "calc(100vw - 2rem)"
        ]
        [ div [ class "fw-semibold bg-body-tertiary border-bottom px-3 py-2" ]
            [ text "Sumarme al grupo" ]
        , case agregarme of
            Nothing ->
                text ""

            Just a ->
                viewAgregarmeForm a
        ]


viewAgregarmeForm : Agregarme -> Html Msg
viewAgregarmeForm a =
    let
        enviando =
            RemoteData.isLoading a.envio
    in
    Html.form
        [ Attr.id "agregarme-form"
        , class "p-3 d-flex align-items-start gap-2"
        , onSubmit (AgregarmeForm Form.Submit)
        ]
        [ div [ class "flex-grow-1" ]
            [ Html.map AgregarmeForm <|
                textInput
                    [ Attr.id "agregarme-nombre"
                    , Attr.placeholder "Nombre"
                    , Attr.attribute "aria-label" "Nombre"
                    , classList
                        [ ( "is-invalid", mensajeDeErrorAlAgregarme a /= Nothing )
                        , ( "bg-body-secondary", enviando )
                        ]

                    -- `readonly` y no `disabled`: un campo deshabilitado
                    -- pierde el foco, y al volver con un error habría que
                    -- hacer click de nuevo para corregirlo.
                    , Attr.readonly enviando
                    ]
                    (Form.getFieldAsString "nombre" a.form)
            , case mensajeDeErrorAlAgregarme a of
                Just error ->
                    div [ class "invalid-feedback" ] [ text error ]

                Nothing ->
                    text ""
            ]
        , Bs.btn Bs.Primary
            [ type_ "submit"
            , Telemetry.click "grupo.agregarme_enviar"

            -- Lo mismo para el botón: se ve deshabilitado pero conserva el
            -- foco. Un segundo submit mientras se envía lo ignora
            -- `enviarAgregarme`.
            , classList [ ( "disabled", enviando ) ]
            , Attr.attribute "aria-disabled"
                (if enviando then
                    "true"

                 else
                    "false"
                )
            ]
            [ text "Aceptar" ]
        ]


textoCompartir : List (Html Msg)
textoCompartir =
    [ i [ class "bi bi-share me-1" ] []
    , text "Compartir"
    ]


{-| Si hay a quién elegir y todavía no se eligió a nadie, la página es la de
bienvenida en vez del contenido del grupo.
-}
eligiendoParticipante : Maybe ULID -> Grupo -> Bool
eligiendoParticipante activeUser grupo =
    activeUser == Nothing && not (List.isEmpty grupo.participantes)


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Msg
viewGlobalUserSelector activeUser grupo =
    viewSelectorDeParticipante
        { attrs = [ class "form-select form-select-sm" ]
        , vacio = option [ value "", selected (activeUser == Nothing) ] [ text "" ]
        , opcion = []
        }
        activeUser
        grupo


{-| El mismo selector, con pinta de botón primario: es lo único que hay para
hacer en la bienvenida. El placeholder no aparece en la lista, porque elegirlo
no haría nada.
-}
viewSelectorDeBienvenida : Maybe ULID -> GrupoLike r -> Html Msg
viewSelectorDeBienvenida activeUser grupo =
    viewSelectorDeParticipante
        { attrs =
            [ class "form-select w-auto bg-primary text-white border-primary"
            , style "cursor" "pointer"
            , style "--bs-form-select-bg-img" "url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%23fff' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\")"
            , Attr.attribute "aria-label" "Participantes de este grupo"
            ]
        , vacio =
            option [ value "", selected (activeUser == Nothing), Attr.hidden True, Attr.disabled True ]
                [ text "Participantes de este grupo" ]
        , opcion = [ class "bg-body text-body" ]
        }
        activeUser
        grupo


viewSelectorDeParticipante :
    { attrs : List (Html.Attribute Msg), vacio : Html Msg, opcion : List (Html.Attribute Msg) }
    -> Maybe ULID
    -> GrupoLike r
    -> Html Msg
viewSelectorDeParticipante config activeUser grupo =
    select
        (on "change"
            (Decode.at [ "target", "value" ] Decode.string
                |> Decode.map
                    (\value ->
                        ForwardSharedMessage <|
                            Shared.SetCurrentParticipante
                                { grupoId = grupo.id
                                , participanteId =
                                    if value == "" then
                                        Nothing

                                    else
                                        Just value
                                }
                    )
            )
            :: config.attrs
        )
        (config.vacio
            :: (grupo.participantes
                    |> List.map
                        (\p ->
                            option
                                (value p.id
                                    :: selected (activeUser == Just p.id)
                                    :: config.opcion
                                )
                                [ text p.nombre ]
                        )
               )
        )


{-| Sin cuenta el rechazo llega como status y no como 'ClaimRejected', pero es
el mismo rechazo.
-}
errorAlAgregarmeSinCuenta : Http.Error -> ErrorAlAgregarme
errorAlAgregarmeSinCuenta error =
    participanteErrorFromHttp error
        |> Maybe.map (Rechazado << ParticipanteError)
        |> Maybe.withDefault FalloElRequest


{-| Por qué no se pudo agregar, si el último intento falló.
-}
mensajeDeErrorAlAgregarme : Agregarme -> Maybe String
mensajeDeErrorAlAgregarme agregarme =
    case agregarme.envio of
        Failure (Rechazado reason) ->
            Just (claimErrorMessage reason)

        Failure FalloElRequest ->
            Just "No pudimos agregarte, probá de nuevo"

        _ ->
            Nothing
