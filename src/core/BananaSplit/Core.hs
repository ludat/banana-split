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
  netosPendientes,
  netosConSaldo,
  ResumenGasto (..),
  resumenGastos2ResumenNetos,
) where

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
import BananaSplit.Moneda (Moneda, PorMoneda, enMoneda, filterPorMoneda)
import BananaSplit.Monto (Monto)
import BananaSplit.Participante (Participante)
import BananaSplit.Repartija (RepartijaClaim (..))
import BananaSplit.TasaDeCambio (TasaDeCambio)
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
-- cómo figura ahí el usuario; el resto del grupo se pide al entrar.
data GrupoParaUsuario = GrupoParaUsuario
  { id :: ULID
  , nombre :: Text
  , participanteNombre :: Text
  }
  deriving (Show, Eq, Generic)

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
    & fmap (\pago -> (calcularNetosPago pago) `enMoneda` pago.moneda)
    & mconcat

-- | Los netos que dejan las transferencias ya hechas. Se suman a los de los
-- pagos porque una transferencia hecha es plata que ya se movió, y por eso
-- sobreviven al descongelar: sin ellas un grupo que se congeló, se saldó y se
-- descongeló volvería a mostrar las deudas que ya se pagaron.
netosDeTransferencias :: PorMoneda [Transferencia] -> PorMoneda (Netos Monto)
netosDeTransferencias =
  fmap (foldMap netosDeTransferencia)

-- | Lo que el grupo todavía se debe: los netos de los gastos menos lo que ya se
-- saldó con transferencias hechas.
netosPendientes :: PorMoneda (Netos Monto) -> PorMoneda [Transferencia] -> PorMoneda (Netos Monto)
netosPendientes netosDeGastos hechas =
  netosDeGastos <> netosDeTransferencias hechas

-- | Saca las monedas en las que ya nadie le debe nada a nadie: no hay deuda que
-- consolidar ni transferencia que sugerir.
netosConSaldo :: PorMoneda (Netos Monto) -> PorMoneda (Netos Monto)
netosConSaldo = filterPorMoneda ((> 0) . deudoresNoNulos)

calcularNetosPago :: Pago -> Netos Monto
calcularNetosPago gasto =
  gasto
    & getResumenGasto
    & netosDeResumenGasto

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

Elm.deriveBoth Elm.defaultOptions ''GrupoParaUsuario
