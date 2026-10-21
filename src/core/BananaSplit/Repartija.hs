{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Repartija where

import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.Monto (Monto)
import BananaSplit.Netos (Netos)
import BananaSplit.Participante (ParticipanteId)
import BananaSplit.ULID (ULID)

data Repartija = Repartija
  { id :: ULID
  , nombre :: Text
  , extra :: Monto
  , distribucionDeSobras :: DistribucionDeSobras
  , items :: [RepartijaItem]
  , claims :: [RepartijaClaim]
  }
  deriving (Show, Eq, Generic)

data ShallowRepartija = ShallowRepartija
  { shallowId :: ULID
  , shallowNombre :: Text
  }
  deriving (Show, Eq, Generic)

data RepartijaItem = RepartijaItem
  { id :: ULID
  , nombre :: Text
  , monto :: Monto
  , cantidad :: Int
  }
  deriving (Show, Eq, Generic)

data RepartijaClaim = RepartijaClaim
  { id :: ULID
  , participante :: ParticipanteId
  , itemId :: ULID
  , cantidad :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data RepartijaForFrontend = RepartijaForFrontend
  { repartija :: Repartija
  , pagoId :: ULID
  , pagoNombre :: Text
  , detalle :: DetalleRepartija
  }
  deriving (Show, Eq, Generic)

-- | Los números de una repartija ya calculados: cuánto consumió cada
-- participante en cada item, cuánto le toca de la propina y de lo que nadie
-- reclamó, y cuánto suma todo eso.
--
-- Los campos no se llaman como los de 'Repartija' —'porItem' y no 'items',
-- 'montoTotal' y no 'monto'— porque un nombre repetido deja ambiguo cualquier
-- record update de los otros tipos.
--
-- Viene con todos los participantes y no con uno solo porque el frontend deja
-- cambiar de quién sos sin volver a pedir la repartija. Lo calcula
-- 'BananaSplit.Deudas.detalleRepartija', que es de donde salen también los
-- netos que se le cobran a cada uno: el frontend muestra estos números en vez
-- de rehacer la cuenta por su cuenta.
data DetalleRepartija = DetalleRepartija
  { porItem :: [DetalleDeItem]
  , propina :: Netos Monto
  , sinReclamar :: DetalleSinReclamar
  , totales :: Netos Monto
  }
  deriving (Show, Eq, Generic)

data DetalleDeItem = DetalleDeItem
  { itemId :: ULID
  , consumos :: Netos Monto
  , montoSinReclamar :: Monto
  , estado :: EstadoDeItem
  , resolucion :: ResolucionDeItem
  }
  deriving (Show, Eq, Generic)

-- | Cómo se está repartiendo un item según sus claims. Es lo que el frontend
-- usa para elegir los botones del item y para contar en qué quedó.
data EstadoDeItem
  = -- | Nadie lo reclamó: todo el item queda sin dueño.
    EstadoSinReclamar
  | -- | Por cantidad: cómo quedan las unidades reclamadas entre todos contra
    -- las que tiene el item.
    EstadoRepartidoPorCantidad DiferenciaDeUnidades
  | -- | En partes iguales entre esta cantidad de personas.
    EstadoRepartidoEquitativamente Int
  | -- | Reclamado por cantidad y en partes iguales a la vez: así no define
    -- ningún reparto y no se le cobra a nadie.
    EstadoRepartoIncorrecto
  deriving (Show, Eq, Generic)

data DiferenciaDeUnidades
  = UnidadesExactas
  | -- | Unidades que no reclamó nadie: lo que valen queda sin dueño.
    FaltanUnidades Int
  | -- | Más unidades reclamadas de las que tiene el item. El monto se reparte
    -- igual, en proporción a lo que reclamó cada uno.
    SobranUnidades Int
  deriving (Show, Eq, Generic)

-- | Si un item deja la repartija lista para repartir.
--
-- 'ItemNoResuelto' es lo que hace que la plata repartida no llegue al total
-- del pago —y por eso la repartija no valga—: un reparto incorrecto, o lo que nadie
-- reclamó cuando no hay a quién pasárselo. 'ItemConAdvertencia' es algo raro
-- que igual se reparte así como está.
data ResolucionDeItem
  = ItemResuelto
  | ItemConAdvertencia
  | ItemNoResuelto
  deriving (Show, Eq, Generic)

-- | Lo que quedó sin dueño: los items que nadie reclamó y las unidades que
-- faltan reclamar en los que se reparten por cantidad. Según
-- 'DistribucionDeSobras' eso se reparte entre los que sí reclamaron algo
-- ('porParticipante') o no se le cobra a nadie, y ahí viene vacío.
data DetalleSinReclamar = DetalleSinReclamar
  { cantidadDeItems :: Int
  , montoTotal :: Monto
  , porParticipante :: Netos Monto
  }
  deriving (Show, Eq, Generic)

data DistribucionDeSobras
  = SobrasNoDistribuir
  | SobrasProporcional
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''DistribucionDeSobras
Elm.deriveBoth Elm.defaultOptions ''DiferenciaDeUnidades
Elm.deriveBoth Elm.defaultOptions ''EstadoDeItem
Elm.deriveBoth Elm.defaultOptions ''ResolucionDeItem
Elm.deriveBoth Elm.defaultOptions ''DetalleDeItem
Elm.deriveBoth Elm.defaultOptions ''DetalleSinReclamar
Elm.deriveBoth Elm.defaultOptions ''DetalleRepartija
Elm.deriveBoth Elm.defaultOptions ''RepartijaItem
Elm.deriveBoth Elm.defaultOptions ''RepartijaClaim
Elm.deriveBoth Elm.defaultOptions ''ShallowRepartija
Elm.deriveBoth Elm.defaultOptions ''Repartija
Elm.deriveBoth Elm.defaultOptions ''RepartijaForFrontend
