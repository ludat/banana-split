# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the frontend for Banana Split, an expense-sharing application. The frontend is built with **Elm** using the **elm-land** framework (v0.20.1) and **Bulma CSS** for styling.

## Development Commands

### Running the development server
```bash
pnpm start
```
This starts both the Elm dev server (elm-land) and CSS watcher concurrently. The dev server runs on port 1234 and proxies `/api` requests to `http://localhost:8000`.

### Building for production
```bash
pnpm build
```
Uses elm-land to create an optimized production build.

### Installing dependencies
```bash
pnpm install
```

## Architecture

### Framework: elm-land

This project uses **elm-land**, a batteries-included framework for Elm applications that provides:
- File-based routing (in `src/Pages/`)
- Layout system (in `src/Layouts/`)
- Shared state management (via `Shared.elm`)
- Effect system for side effects

### Generated API Types

**IMPORTANT**: The file `src/Generated/Api.elm` is generated from the Haskell backend using `servant-elm`.
- **Do not manually edit this file** - changes will be overwritten
- To update API types: regenerate from the backend using the Haskell executable
- This file contains all type definitions and HTTP request functions for communicating with the backend

### Key Architecture Patterns

#### 1. The Store Pattern
The application uses a centralized store for caching API data, implemented in `src/Models/Store.elm`. The store:
- Caches grupos, pagos, repartijas, and resumenes by ID
- Uses `RemoteData` (WebData) to track loading states
- Provides `ensure*` functions to fetch data only if not already loaded
- Provides `refresh*` functions to force-reload data
- Uses `StoreMsg` sent through Shared state to update the cache

Example usage in a page:
```elm
init : ULID -> ( Model, Effect Msg )
init grupoId =
    ( model
    , Models.Store.ensureGrupo grupoId store  -- Fetches only if needed
    )
```

#### 2. Shared State
Global state is managed through `Shared.elm` and `Shared.Model`:
- `store`: The centralized data cache (Store type)
- `userId`: Currently selected participant (Maybe ULID)
- `toasties`: Toast notifications

Pages send messages to Shared via `Effect.sendSharedMsg` or `Effect.sendStoreMsg`.

#### 3. Layouts
The `src/Layouts/Default.elm` layout provides:
- Navigation bar (with mobile burger menu)
- User selection interface (when no user is selected)
- Toast notification container
- Common page chrome

Layouts receive props from pages and can have their own Model/Msg.

#### 4. Effects System
The `Effect.elm` module wraps Elm commands and provides helpers:
- `Effect.sendSharedMsg`: Send message to Shared state
- `Effect.sendStoreMsg`: Send message to Store (shorthand for StoreMsg)
- `Effect.sendToastMsg`: Send toast notification message
- `Effect.pushRoutePath`: Navigate to a new route

#### 5. Routing
Routes are file-based in `src/Pages/`:
- `Home_.elm` → `/`
- `Grupos/Id_.elm` → `/grupos/:id`
- `Grupos/GrupoId_/Pagos/New.elm` → `/grupos/:grupoId/pagos/new`

Dynamic segments use `_` suffix (e.g., `Id_`, `GrupoId_`).

### Directory Structure

```
src/
├── Components/       # Reusable UI components (NavBar, BarrasDeNetos)
├── Css.elm          # Generated CSS-in-Elm styles from styles.css
├── Effect.elm       # Effect system for side effects
├── Generated/       # DO NOT EDIT - generated from backend
│   └── Api.elm      # API types and functions (servant-elm)
├── Layouts/         # Layout components
│   └── Default.elm  # Main app layout with nav and toasts
├── Models/          # Domain models and store
│   ├── Grupo.elm
│   ├── Monto.elm
│   ├── Pago.elm
│   └── Store.elm    # Centralized data cache
├── Pages/           # File-based routes (elm-land)
│   ├── Home_.elm
│   └── Grupos/
├── Shared.elm       # Global state management
├── Utils/           # Utility modules
│   ├── Form.elm     # Form helpers
│   ├── Http.elm     # HTTP utilities
│   ├── Toasts.elm   # Toast notification system
│   └── Ulid.elm     # ULID utilities
└── styles.css       # Source CSS (compiled to Css.elm)
```

### State Management Flow

1. Page receives route params in `init`
2. Page uses `Models.Store.ensure*` functions to load data
3. Store checks cache, sends HTTP request if needed via `Effect.sendStoreMsg`
4. Response updates Store in Shared state
5. Page accesses data via `Store.get*` functions using `shared.store`

### Common Patterns

#### Creating a new page with data loading
```elm
page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route.params.id shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout (toLayout shared.store route.params.id)

init : ULID -> Store -> () -> ( Model, Effect Msg )
init grupoId store () =
    ( model
    , Effect.batch
        [ Models.Store.ensureGrupo grupoId store
        , Models.Store.ensureResumen grupoId store
        ]
    )

view : Store -> Model -> View Msg
view store model =
    case Models.Store.getGrupo grupoId store of
        Success grupo ->
            -- render with data
        Loading ->
            -- render loading state
        -- etc
```

#### Refreshing data after mutation
```elm
case response of
    Ok pago ->
        ( model
        , Effect.batch
            [ Models.Store.refreshGrupo grupoId  -- Force reload
            , Effect.pushRoutePath targetRoute
            ]
        )
```

#### Adding toast notifications
```elm
Effect.sendToastMsg (AddToast { content = "Pago created!", level = ToastSuccess })
```

## Important Notes

- **RemoteData**: Use `RemoteData.WebData` for all API data to properly handle NotAsked/Loading/Failure/Success states
- **ULIDs**: The app uses ULIDs (String type) for all entity IDs
- **Forms**: Use `elm-simple-form` library (via `Utils.Form`) for form handling
- **Styling**: Uses Bulma CSS classes. Custom styles go in `styles.css` and are processed by `@ryannhg/css-in-elm`
- **API proxy**: The dev server proxies `/api` to `localhost:8000` (configured in `elm-land.json`)
