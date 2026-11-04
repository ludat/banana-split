module Components.Landing exposing (view)

{-| La página de presentación: lo que ve en `/` quien no inició sesión, y
cualquiera en `/landing`. Es pura vista; el botón de "Comenzá" lo resuelve la
página que la muestra (ver `Pages.Home_`).
-}

import Components.Bootstrap as Bs
import Html exposing (Html, a, div, h1, h2, h3, i, img, li, p, section, small, text, ul)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import Route.Path as Path
import Utils.Telemetry as Telemetry


repositorio : String
repositorio =
    "https://github.com/ludat/banana-split"


view : { onComenzar : msg } -> Html msg
view config =
    div [ Attr.id "landing" ]
        [ Telemetry.seccionVista "landing.hero" (viewHero config)
        , Telemetry.seccionVista "landing.destacados" viewDestacados
        , Telemetry.seccionVista "landing.casos_de_uso" viewCasosDeUso
        , Telemetry.seccionVista "landing.funciones" viewFunciones
        , Telemetry.seccionVista "landing.aporte" viewAporte
        , Telemetry.seccionVista "landing.codigo_abierto" viewCodigoAbierto
        , Telemetry.seccionVista "landing.footer" viewFooter
        ]


viewHero : { onComenzar : msg } -> Html msg
viewHero { onComenzar } =
    section [ class "container py-5" ]
        [ div [ class "row align-items-center g-5" ]
            [ div [ class "col-12 col-lg-7" ]
                [ Bs.badge "text-bg-secondary rounded-pill mb-3" [] [ text "Menos matemáticas, más momentos" ]
                , h1 [ class "display-4 mb-3" ] [ text "Dividí gastos con amigos" ]
                , p [ class "lead text-body-secondary" ]
                    [ text "Gestioná gastos de manera colaborativa y manejá grupos con muchos participantes." ]
                , ul [ class "text-body-secondary small mb-4" ]
                    [ li [] [ text "Salidas ocasionales o encuentros regulares" ]
                    , li [] [ text "Viajes largos con gastos en distintas monedas" ]
                    , li [] [ text "Cálculos y divisiones imposibles" ]
                    ]
                , Bs.btn Bs.Primary
                    [ Attr.id "landing-comenzar"
                    , class "btn-lg"
                    , Telemetry.click "landing.comenzar"
                    , onClick onComenzar
                    ]
                    [ i [ class "bi bi-rocket-takeoff me-2" ] []
                    , text "Comenzá: creá tu primer grupo"
                    ]
                , div [ class "form-text" ] [ text "No requiere registro y es de uso gratuito" ]
                ]
            , div [ class "col-12 col-lg-5" ]
                [ img
                    [ class "img-fluid rounded-4 shadow"
                    , Attr.src "/landing/perros-poker.jpg"
                    , Attr.alt "Perros jugando al póker alrededor de una mesa"
                    , Attr.width 800
                    , Attr.height 540
                    ]
                    []
                ]
            ]
        ]


viewDestacados : Html msg
viewDestacados =
    let
        destacado icono titulo descripcion =
            div [ class "col-12 col-md-4" ]
                [ div [ class "h-100 p-4 rounded-3 bg-body-tertiary text-center" ]
                    [ i [ class ("bi fs-2 text-primary " ++ icono) ] []
                    , h3 [ class "h6 mt-2" ] [ text titulo ]
                    , p [ class "small text-body-secondary mb-0" ] [ text descripcion ]
                    ]
                ]
    in
    section [ class "container pb-5" ]
        [ div [ class "row g-3" ]
            [ destacado "bi-robot" "Lee recibos con IA" "Cargá un gasto grupal a partir de una foto del recibo"
            , destacado "bi-lightning-charge-fill" "Empezá sin registrarte" "Creá un grupo, sumá participantes e invitá a colaborar sin registrarte"
            , destacado "bi-people-fill" "Invitá a colaborar" "Todos los participantes pueden ver y ayudar en la gestión del grupo"
            ]
        , div [ class "d-flex justify-content-center mt-4" ]
            [ div [ class "alert alert-warning py-2 px-4 mb-0 small" ]
                [ i [ class "bi bi-emoji-sunglasses me-2" ] []
                , text "Ideal para los que tienen memoria selectiva"
                ]
            ]
        ]


viewCasosDeUso : Html msg
viewCasosDeUso =
    let
        caso icono titulo descripcion =
            Bs.card [ class "shadow-sm overflow-hidden mb-4" ]
                [ div [ class "row g-0" ]
                    [ div [ class "col-12 col-md-4 bg-dark text-white d-flex align-items-center justify-content-center py-5" ]
                        [ i [ class ("bi display-1 " ++ icono) ] [] ]
                    , div [ class "col-12 col-md-8" ]
                        [ Bs.cardBody [ class "p-4" ]
                            [ Bs.badge "text-bg-primary" [] [ text "Casos de uso" ]
                            , h2 [ class "h2 mt-2" ] [ text titulo ]
                            , p [ class "text-body-secondary mb-0" ] [ text descripcion ]
                            ]
                        ]
                    ]
                ]
    in
    section [ class "bg-body-tertiary py-5" ]
        [ div [ class "container" ]
            [ caso "bi-people"
                "Salidas grupales grandes"
                "Manejá gran cantidad de participantes de manera rápida y práctica. Colaborá mediante repartijas, visualizá información de resúmenes y gráficas automáticas que te facilitan el control."
            , caso "bi-airplane"
                "Viajes largos sin límites"
                "Cargá gastos en distintas monedas y navegá la información en una moneda común. Calculá las transferencias entre participantes sin complicarte con las cuentas."
            ]
        ]


viewFunciones : Html msg
viewFunciones =
    let
        principal titulo descripcion =
            div [ class "col-12 col-md-6" ]
                [ div [ class "h-100 p-4 rounded-3 bg-body-tertiary" ]
                    [ Bs.badge "text-bg-primary" [] [ text "Colaboración" ]
                    , h3 [ class "h4 mt-2" ] [ text titulo ]
                    , p [ class "text-body-secondary mb-0" ] [ text descripcion ]
                    ]
                ]

        funcion titulo descripcion =
            div [ class "col-12 col-md-6 col-lg-4" ]
                [ div [ class "h-100 p-3 rounded-3 bg-body-tertiary" ]
                    [ h3 [ class "h6" ] [ text titulo ]
                    , p [ class "small text-body-secondary mb-0" ] [ text descripcion ]
                    ]
                ]
    in
    section [ class "container py-5" ]
        [ h2 [ class "display-6 mb-4" ] [ text "Funciones" ]
        , div [ class "row g-3 mb-3" ]
            [ principal "Gestión grupal" "Cualquier participante puede realizar acciones en nombre de los demás y así ayudarse entre todos en la carga de los gastos."
            , principal "Ingreso sin barreras" "Todos pueden hacer lo importante antes de registrarse: crear el grupo, cargar gastos y ver cómo vienen las cuentas."
            ]
        , div [ class "row g-3" ]
            [ funcion "Repartos colaborativos" "Mandá un enlace al grupo para que cada uno registre qué consumió."
            , funcion "Modo saldar deudas" "Calculá las transferencias que deben hacerse entre los participantes."
            , funcion "Balances en el momento" "Cálculos de resumen, estados y analíticas actualizadas siempre."
            , funcion "Gastos en distintas monedas" "Pesos, dólares o reales en el mismo grupo."
            , funcion "Gráficas de datos" "Visualizá en gráficos quién puso y quién debe."
            , funcion "En cualquier dispositivo" "Accedé desde el navegador o desde el celular."
            , funcion "Carga de recibos por email" "Reenviá un recibo digital para cargarlo automáticamente."
            , funcion "Instalable en tu celular" "Instalala como cualquier otra app, directo desde el navegador."
            , funcion "Cuentas al cargar" "Escribí `=` en un monto para hacer la cuenta ahí mismo."
            ]
        ]


viewAporte : Html msg
viewAporte =
    section [ class "bg-body-tertiary py-5" ]
        [ div [ class "container text-center" ]
            [ img
                [ class "img-fluid rounded-4 shadow mb-4"
                , Attr.src "/landing/perros-brindis.jpg"
                , Attr.alt "Perros brindando alrededor de una mesa"
                , Attr.attribute "loading" "lazy"
                , Attr.width 1200
                , Attr.height 819
                , Attr.style "max-width" "32rem"
                ]
                []
            , p [ class "lead text-body-secondary mx-auto mb-0", Attr.style "max-width" "40rem" ]
                [ text "Sabemos que no somos la primera aplicación de este tipo, pero buscamos hacer un aporte a este género." ]
            ]
        ]


viewCodigoAbierto : Html msg
viewCodigoAbierto =
    let
        tarjeta etiqueta titulo descripcion =
            div [ class "col-12 col-md-6" ]
                [ Bs.card [ class "h-100 shadow-sm" ]
                    [ Bs.cardBody []
                        [ Bs.badge "text-bg-primary" [] [ text etiqueta ]
                        , h3 [ class "h5 mt-2" ] [ text titulo ]
                        , p [ class "small text-body-secondary mb-0" ] [ text descripcion ]
                        ]
                    ]
                ]
    in
    section [ class "container py-5" ]
        [ h2 [ class "display-6 mb-3" ] [ text "Código abierto" ]
        , p [ class "text-body-secondary", Attr.style "max-width" "40rem" ]
            [ text "Banana Split es una aplicación de código abierto que comenzó en 2024. Todo el código está publicado, y cualquiera puede leerlo, proponer cambios o correr su propia instancia." ]
        , div [ class "d-flex flex-wrap gap-2 mb-4" ]
            [ a
                [ class "btn btn-primary btn-sm"
                , Attr.href repositorio
                , Attr.target "_blank"
                , Attr.rel "noopener"
                , Telemetry.click "landing.repositorio"
                ]
                [ i [ class "bi bi-github me-1" ] [], text "Repositorio" ]
            , a
                [ class "btn btn-outline-primary btn-sm"
                , Attr.href repositorio
                , Attr.target "_blank"
                , Attr.rel "noopener"
                , Telemetry.click "landing.valorar"
                ]
                [ i [ class "bi bi-star-fill me-1" ] [], text "Valorá el proyecto" ]
            ]
        , div [ class "row g-3" ]
            [ tarjeta "Siempre bienvenido" "Aportá código" "Los pull requests son bienvenidos: arreglos, funciones nuevas o mejoras a lo que ya existe."
            , tarjeta "Comunidad" "Reportá problemas" "Si algo no anda o se te ocurre una idea, abrí un issue en el repositorio."
            , tarjeta "Gastos mensuales" "Aportá a la infraestructura" "Mantener la app en línea tiene un costo; cualquier ayuda para sostenerla suma."
            , tarjeta "Comunidad" "Valorá el proyecto" "Una estrella en GitHub ayuda a que más gente lo encuentre."
            ]
        ]


viewFooter : Html msg
viewFooter =
    Html.footer [ class "border-top bg-body-tertiary py-5" ]
        [ div [ class "container" ]
            [ div [ class "row g-4" ]
                [ div [ class "col-12 col-md-6" ]
                    [ div [ class "h4 mb-1" ] [ text "Banana Split" ]
                    , small [ class "text-body-secondary d-block mb-2" ] [ text "Aplicación de gastos compartidos" ]
                    , a [ class "text-reset fs-5", Attr.href repositorio, Attr.target "_blank", Attr.rel "noopener", Attr.attribute "aria-label" "GitHub", Telemetry.click "landing.footer_github" ]
                        [ i [ class "bi bi-github" ] [] ]
                    ]
                , div [ class "col-6 col-md-3" ]
                    [ ul [ class "list-unstyled small mb-0" ]
                        [ li [ class "mb-1" ] [ a [ class "link-secondary", Path.href Path.Login, Telemetry.click "landing.footer_login" ] [ text "Iniciar sesión" ] ]
                        , li [ class "mb-1" ] [ a [ class "link-secondary", Path.href Path.Home_, Telemetry.click "landing.footer_inicio" ] [ text "Inicio" ] ]
                        ]
                    ]
                , div [ class "col-6 col-md-3" ]
                    [ ul [ class "list-unstyled small mb-0" ]
                        [ li [ class "mb-1" ] [ a [ class "link-secondary", Attr.href repositorio, Attr.target "_blank", Attr.rel "noopener", Telemetry.click "landing.footer_codigo" ] [ text "Código fuente" ] ]
                        , li [ class "mb-1" ] [ a [ class "link-secondary", Attr.href (repositorio ++ "/issues"), Attr.target "_blank", Attr.rel "noopener", Telemetry.click "landing.footer_issues" ] [ text "Reportar un problema" ] ]
                        ]
                    ]
                ]
            ]
        ]
