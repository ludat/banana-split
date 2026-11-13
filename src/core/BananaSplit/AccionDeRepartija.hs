{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Ninguna acción cambia lo que suma la repartija (sus items más la propina),
-- así que el gasto sigue cerrando sin tocarlo: lo que un item gana o pierde
-- sale de la propina o vuelve a ella.
module BananaSplit.AccionDeRepartija (
  AccionDeRepartija (..),
  AccionDeRepartijaInvalida (..),
  aplicarAccionDeRepartija,
  escalarMonto,
) where

import Data.Text qualified as Text
import Elm.Derive qualified as Elm

import BananaSplit.Monto (Monto, getLugaresDespuesDeLaComa, mantisaEn, mkMonto, times)
import BananaSplit.Repartija (Repartija (..), RepartijaClaim (..), RepartijaItem (..))
import BananaSplit.ULID (ULID, nullUlid)
import Preludat

data AccionDeRepartija
  = AgregarUnidades ULID Int
  | QuitarUnidades ULID Int
  | SepararUnidades ULID Int Text
  | RepartirProporcionalmente ULID
  | -- | Nombre, monto total (no el de cada unidad) y unidades.
    AgregarItem Text Monto Int
  | -- | Junta el segundo item en el primero, que queda con este nombre. Ver
    -- 'unirClaims' para qué pasa con los reclamos.
    UnirItems ULID ULID Text
  deriving (Show, Eq, Generic)

data AccionDeRepartijaInvalida
  = ItemNoEncontrado
  | ItemSinUnidades
  | CantidadDeUnidadesInvalida
  | -- | Sin items la repartija deja de valer.
    NoPuedeQuedarSinItems
  | UnirConElMismoItem
  | ItemSinNombre
  deriving (Show, Eq, Generic)

-- | @nuevoItemId@ es el id del item que crean 'SepararUnidades' y 'AgregarItem'.
aplicarAccionDeRepartija ::
  ULID -> AccionDeRepartija -> Repartija -> Either AccionDeRepartijaInvalida Repartija
aplicarAccionDeRepartija nuevoItemId accion repartija =
  case accion of
    AgregarUnidades itemId n -> do
      item <- buscarItem itemId
      conUnidades item
      when (n <= 0) $ Left CantidadDeUnidadesInvalida
      cambiarCantidad item (item.cantidad + n)
    QuitarUnidades itemId n -> do
      item <- buscarItem itemId
      validarQueQuedeAlgo item n
      cambiarCantidad item (item.cantidad - n)
    SepararUnidades itemId n nombre -> do
      item <- buscarItem itemId
      validarQueQuedeAlgo item n
      nombreDelSeparado <- validarNombre nombre
      let restante = conCantidad item (item.cantidad - n)
          -- Lo que vale lo separado sale por diferencia y no escalando, así el
          -- redondeo no puede hacer que entre los dos no sumen lo de antes.
          separado =
            RepartijaItem
              { id = nuevoItemId
              , nombre = nombreDelSeparado
              , monto = item.monto - restante.monto
              , cantidad = n
              }
      pure $ con repartija.extra (reemplazarPor item [restante, separado]) repartija.claims
    RepartirProporcionalmente itemId -> do
      item <- buscarItem itemId
      when (length repartija.items <= 1) $ Left NoPuedeQuedarSinItems
      pure $ con (repartija.extra + item.monto) (reemplazarPor item []) (sinClaimsDe [itemId])
    AgregarItem nombre monto cantidad -> do
      nombreDelNuevo <- validarNombre nombre
      when (cantidad <= 0) $ Left CantidadDeUnidadesInvalida
      let nuevo =
            RepartijaItem
              { id = nuevoItemId
              , nombre = nombreDelNuevo
              , monto = monto
              , cantidad = cantidad
              }
      pure $ con (repartija.extra - monto) (repartija.items <> [nuevo]) repartija.claims
    UnirItems itemId otroId nombre -> do
      when (otroId == itemId) $ Left UnirConElMismoItem
      item <- buscarItem itemId
      otro <- buscarItem otroId
      nombreDelUnido <- validarNombre nombre
      let unido =
            RepartijaItem
              { id = item.id
              , nombre = nombreDelUnido
              , monto = item.monto + otro.monto
              , cantidad = item.cantidad + otro.cantidad
              }
          items =
            repartija.items
              & filter (\i -> i.id /= otroId)
              & fmap (\i -> if i.id == itemId then unido else i)
          claims =
            sinClaimsDe [itemId, otroId] <> unirClaims item otro
      pure $ con repartija.extra items claims
  where
    buscarItem i =
      maybeToRight ItemNoEncontrado $ find (\candidato -> candidato.id == i) repartija.items

    -- La propina puede quedar negativa: es un descuento que se reparte igual
    -- que una propina.
    cambiarCantidad :: RepartijaItem -> Int -> Either AccionDeRepartijaInvalida Repartija
    cambiarCantidad item cantidad = do
      let nuevo = conCantidad item cantidad
          extra = repartija.extra - (nuevo.monto - item.monto)
      pure $ con extra (reemplazarPor item [nuevo]) repartija.claims

    conCantidad :: RepartijaItem -> Int -> RepartijaItem
    conCantidad item cantidad =
      RepartijaItem
        { id = item.id
        , nombre = item.nombre
        , monto = escalarMonto item.cantidad cantidad item.monto
        , cantidad = cantidad
        }

    reemplazarPor :: RepartijaItem -> [RepartijaItem] -> [RepartijaItem]
    reemplazarPor item nuevos =
      repartija.items & concatMap (\i -> if i.id == item.id then nuevos else [i])

    conUnidades :: RepartijaItem -> Either AccionDeRepartijaInvalida ()
    conUnidades item =
      when (item.cantidad <= 0) $ Left ItemSinUnidades

    validarQueQuedeAlgo :: RepartijaItem -> Int -> Either AccionDeRepartijaInvalida ()
    validarQueQuedeAlgo item n = do
      conUnidades item
      when (n <= 0 || n >= item.cantidad) $ Left CantidadDeUnidadesInvalida

    sinClaimsDe ids = repartija.claims & filter (\c -> c.itemId `notElem` ids)

    validarNombre :: Text -> Either AccionDeRepartijaInvalida Text
    validarNombre nombre =
      let limpio = Text.strip nombre
      in if Text.null limpio then Left ItemSinNombre else Right limpio

    con :: Monto -> [RepartijaItem] -> [RepartijaClaim] -> Repartija
    con extra items claims =
      Repartija
        { id = repartija.id
        , nombre = repartija.nombre
        , extra = extra
        , distribucionDeSobras = repartija.distribucionDeSobras
        , items = items
        , claims = claims
        , noConsumieron = repartija.noConsumieron
        }

    -- Si las unidades de los dos valen distinto, una unidad del item unido no
    -- significa nada (depende de cuál venía), así que juntarlos es pensarlos
    -- para repartir en partes iguales: participan, en partes iguales, todos los
    -- que reclamaban alguno.
    --
    -- Si valen lo mismo se juntan cuando los dos se reparten igual: todos por
    -- cantidad (a cada uno se le suman sus unidades) o todos en partes iguales
    -- (participan todos los que participaban en alguno). Si unos son por
    -- cantidad y otros en partes iguales no hay cómo juntarlos sin inventar un
    -- reparto, y el item queda sin reclamar.
    --
    -- Cada participante se queda con el id de su claim en el item que queda, si
    -- lo tenía; los que vienen del otro item son nuevos, porque el claim viejo
    -- se borra con su item.
    unirClaims :: RepartijaItem -> RepartijaItem -> [RepartijaClaim]
    unirClaims item otro
      | not mismoPrecioUnitario = porParticipante (const Nothing)
      | all (isJust . (.cantidad)) todos = porParticipante (fmap sum . sequence)
      | all (isNothing . (.cantidad)) todos = porParticipante (const Nothing)
      | otherwise = []
      where
        deEste = repartija.claims & filter (\c -> c.itemId == item.id)
        delOtro = repartija.claims & filter (\c -> c.itemId == otro.id)
        todos = deEste <> delOtro

        -- Se compara en cruz para no redondear el precio por unidad.
        mismoPrecioUnitario =
          item.monto `times` otro.cantidad == otro.monto `times` item.cantidad

        porParticipante :: ([Maybe Int] -> Maybe Int) -> [RepartijaClaim]
        porParticipante combinar =
          todos
            & fmap (.participante)
            & ordNub
            & fmap
              ( \p ->
                  RepartijaClaim
                    { id = maybe nullUlid (.id) (find (\c -> c.participante == p) deEste)
                    , participante = p
                    , itemId = item.id
                    , cantidad =
                        todos & filter (\c -> c.participante == p) & fmap (.cantidad) & combinar
                    }
              )

escalarMonto :: Int -> Int -> Monto -> Monto
escalarMonto cantidad nuevaCantidad monto =
  let lugares = getLugaresDespuesDeLaComa monto
      mantisa = mantisaEn lugares monto
  in mkMonto lugares
       $ round (mantisa * fromIntegral nuevaCantidad % fromIntegral cantidad)

Elm.deriveBoth Elm.defaultOptions ''AccionDeRepartija
Elm.deriveBoth Elm.defaultOptions ''AccionDeRepartijaInvalida
