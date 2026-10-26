{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Core (
  Grupo (..),
  GrupoParaUsuario (..),
  EstadoParaUsuario (..),
  AbiertoParaUsuario (..),
  CongeladoParaUsuario (..),
  netoConsolidadoDe,
  pendienteDe,
  estaCongelado,
  nullUlid,
  -- Pago
  Distribucion (..),
  Pago (..),
  ShallowPago (..),
  TipoDistribucion (..),
  calcularNetosPago,
  calcularNetosTotales,
  gastoEsValido,
  resumenGastoEsValido,
  getResumenGasto,
  netosDeResumenGasto,
  netosDeTransferencias,
  netosConSaldo,
  ResumenGasto (..),
  resumenGastos2ResumenNetos,
) where

import Data.Map.Strict qualified as Map
import Data.Time (Day, UTCTime)
import Elm.Derive qualified as Elm
import Elm.TyRep (
  EPrimAlias (..),
  ETCon (..),
  EType (..),
  ETypeDef (..),
  ETypeName (..),
  IsElmDefinition (..),
 )

import BananaSplit.Deudas
import BananaSplit.Moneda (Moneda, PorMoneda (..), enMoneda, filterConMoneda, filterPorMoneda)
import BananaSplit.Monto (Monto)
import BananaSplit.Netos (Netos (..))
import BananaSplit.Participante (Participante, ParticipanteId)
import BananaSplit.PorParticipante qualified as PorParticipante
import BananaSplit.Repartija (RepartijaClaim (..))
import BananaSplit.TasaDeCambio (ConsolidadoNetos (..), TablaDeTasas, TasaDeCambio, consolidarNetos)
import BananaSplit.ULID
import Preludat

data Grupo = Grupo
  { id :: ULID
  , nombre :: Text
  , participantes :: [Participante]
  , congeladoAt :: Maybe UTCTime
  , monedaPorDefecto :: Moneda
  , tasasDeCambio :: [TasaDeCambio]
  , monedasConPagos :: [Moneda]
  }
  deriving (Show, Eq, Generic)

estaCongelado :: Grupo -> Bool
estaCongelado grupo = isJust grupo.congeladoAt

-- | Un grupo en la lista de "mis grupos". Alcanza con el nombre del grupo y con
-- lo que más le importa al usuario según en qué anda el grupo; el resto se
-- pide al entrar.
data GrupoParaUsuario = GrupoParaUsuario
  { id :: ULID
  , nombre :: Text
  , monedaPorDefecto :: Moneda
  , estado :: EstadoParaUsuario
  }
  deriving (Show, Eq, Generic)

data EstadoParaUsuario
  = EnGrupoAbierto AbiertoParaUsuario
  | EnGrupoCongelado CongeladoParaUsuario
  deriving (Show, Eq, Generic)

-- | Mientras el grupo está abierto lo que importa es cómo venís: tu neto, sin
-- las monedas en las que quedaste en cero.
data AbiertoParaUsuario = AbiertoParaUsuario
  { neto :: PorMoneda Monto
  , cantidadPagos :: Int
  , cantidadPagosInvalidos :: Int
  }
  deriving (Show, Eq, Generic)

-- | Congelado, las deudas ya son transferencias: lo que importa es cuánto te
-- falta mover. Las dos puntas por separado, porque una transferencia cargada a
-- mano te puede poner de los dos lados.
data CongeladoParaUsuario = CongeladoParaUsuario
  { aCobrar :: PorMoneda Monto
  , aPagar :: PorMoneda Monto
  }
  deriving (Show, Eq, Generic)

-- | El neto de un participante tal como lo muestra la página del grupo:
-- consolidado en la moneda por defecto. Las monedas sin tasa no hay cómo
-- convertirlas, así que quedan aparte en la suya.
--
-- Consolida los netos de todo el grupo y recién después mira los de uno,
-- porque el redondeo de la conversión se reparte entre todos: convertir solo
-- el suyo podría dar unos centavos distinto de lo que ve adentro del grupo.
netoConsolidadoDe :: ParticipanteId -> TablaDeTasas -> PorMoneda (Netos Monto) -> PorMoneda Monto
netoConsolidadoDe participante tabla netos =
  let
    consolidado = consolidarNetos tabla (netosConSaldo netos)
    sinConvertir = netos & filterConMoneda (\moneda _ -> moneda `elem` consolidado.monedasSinTasa)
  in
    (consolidado.netos `enMoneda` consolidado.moneda)
      <> sinConvertir
      & fmap (\(Netos porParticipante) -> PorParticipante.buscar participante porParticipante)
      & catMaybesPorMoneda
      & filterPorMoneda (/= 0)
  where
    catMaybesPorMoneda (PorMoneda mapa) = PorMoneda (Map.mapMaybe identity mapa)

-- | Lo que le falta cobrar y pagar a un participante en un grupo congelado:
-- la suma de sus transferencias pendientes, por moneda.
pendienteDe :: ParticipanteId -> [Transferencia] -> CongeladoParaUsuario
pendienteDe participante transferencias =
  CongeladoParaUsuario
    { aCobrar = totalDondeEs (.to)
    , aPagar = totalDondeEs (.from)
    }
  where
    totalDondeEs punta =
      transferencias
        & filter (\t -> not (transferenciaEstaHecha t) && punta t == participante)
        & fmap (\t -> (t.moneda, t.monto))
        & Map.fromListWith (+)
        & PorMoneda

data Pago = Pago
  { pagoId :: ULID
  , monto :: Monto
  , moneda :: Moneda
  , nombre :: Text
  , fecha :: Day
  , pagadores :: Distribucion
  , deudores :: Distribucion
  }
  deriving (Show, Eq, Generic)

data ShallowPago = ShallowPago
  { pagoId :: ULID
  , nombre :: Text
  , monto :: Monto
  , moneda :: Moneda
  , fecha :: Day
  , resumen :: ResumenGasto
  }
  deriving (Show, Eq, Generic)

-- | Los netos que dejan todos los gastos de un grupo, por moneda. Los
-- inválidos no cuentan: no se sabe quién puso ni quién consumió.
calcularNetosTotales :: [Pago] -> PorMoneda (Netos Monto)
calcularNetosTotales pagos =
  pagos
    & filter gastoEsValido
    & fmap calcularNetosPago
    & mconcat

netosDeTransferencias :: [Transferencia] -> PorMoneda (Netos Monto)
netosDeTransferencias transferencias =
  transferencias
    & filter transferenciaEstaHecha
    & foldMap netosDeTransferencia

netosConSaldo :: PorMoneda (Netos Monto) -> PorMoneda (Netos Monto)
netosConSaldo = filterPorMoneda ((> 0) . deudoresNoNulos)

calcularNetosPago :: Pago -> PorMoneda (Netos Monto)
calcularNetosPago gasto =
  gasto
    & getResumenGasto
    & netosDeResumenGasto
    & (`enMoneda` gasto.moneda)

data ResumenGasto = ResumenGasto
  { pagado :: Netos Monto
  , consumido :: Netos Monto
  , errores :: [ErrorResumen]
  , participantesEnRepartija :: Maybe Int
  }
  deriving (Show, Eq, Generic)

resumenGastos2ResumenNetos :: ResumenGasto -> ResumenNetos
resumenGastos2ResumenNetos resumen =
  ResumenNetos
    { netos = resumen.pagado <> fmap negate resumen.consumido
    , total = totalNetos resumen.pagado
    , errores = resumen.errores
    }

getResumenGasto :: Pago -> ResumenGasto
getResumenGasto pago =
  let
    resumenPagadores = getResumen pago.monto pago.pagadores
    resumenDeudores = getResumen pago.monto pago.deudores
  in
    ResumenGasto
      { pagado = resumenPagadores.netos
      , consumido = resumenDeudores.netos
      , errores =
          fmap (relabelError "pagadores") resumenPagadores.errores
            <> fmap (relabelError "deudores") resumenDeudores.errores
      , participantesEnRepartija = case pago.deudores.tipo of
          TipoDistribucionRepartija repartija ->
            repartija.claims
              & fmap (.participante)
              & ordNub
              & length
              & Just
          _ -> Nothing
      }

netosDeResumenGasto :: ResumenGasto -> Netos Monto
netosDeResumenGasto resumen =
  resumen.pagado <> fmap negate resumen.consumido

gastoEsValido :: Pago -> Bool
gastoEsValido pago =
  pago
    & getResumenGasto
    & resumenGastoEsValido

resumenGastoEsValido :: ResumenGasto -> Bool
resumenGastoEsValido resumen =
  null resumen.errores

instance IsElmDefinition UTCTime where
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias{epa_name = ETypeName{et_name = "UTCTime", et_args = []}, epa_type = ETyCon (ETCon{tc_name = "String"})})

instance IsElmDefinition Day where
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias{epa_name = ETypeName{et_name = "Day", et_args = []}, epa_type = ETyCon (ETCon{tc_name = "String"})})

Elm.deriveBoth Elm.defaultOptions ''Pago
Elm.deriveBoth Elm.defaultOptions ''ResumenGasto
Elm.deriveBoth Elm.defaultOptions ''ShallowPago
Elm.deriveBoth Elm.defaultOptions ''Grupo

Elm.deriveBoth Elm.defaultOptions ''AbiertoParaUsuario
Elm.deriveBoth Elm.defaultOptions ''CongeladoParaUsuario
Elm.deriveBoth Elm.defaultOptions ''EstadoParaUsuario
Elm.deriveBoth Elm.defaultOptions ''GrupoParaUsuario
