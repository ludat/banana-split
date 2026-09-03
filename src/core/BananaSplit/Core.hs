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
  ShallowGrupo (..),
  GrupoParaUsuario (..),
  estaCongelado,
  nullUlid,
  -- Pago
  Distribucion (..),
  Pago (..),
  ShallowPago (..),
  TipoDistribucion (..),
  addIsValidPago,
  calcularNetosPago,
  calcularNetosTotales,
  getResumenPago,
  isValid,
  netosDeTransacciones,
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
import BananaSplit.Moneda (Moneda, PorMoneda, enMoneda)
import BananaSplit.Monto (Monto)
import BananaSplit.Participante (Participante)
import BananaSplit.TasaDeCambio (TasaDeCambio)
import BananaSplit.ULID
import Preludat

data Grupo = Grupo
  { id :: ULID
  , nombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  , monedaPorDefecto :: Moneda
  }
  deriving (Show, Eq, Generic)

data ShallowGrupo = ShallowGrupo
  { id :: ULID
  , nombre :: Text
  , participantes :: [Participante]
  , congeladoAt :: Maybe UTCTime
  -- ^ Cuándo se congeló el grupo, o 'Nothing' si está descongelado. La fecha
  -- también distingue las transacciones hechas durante este congelamiento de
  -- las que arrastra de los anteriores.
  , monedaPorDefecto :: Moneda
  , tasasDeCambio :: [TasaDeCambio]
  , monedasConPagos :: [Moneda]
  -- ^ Las monedas en las que hay pagos cargados. Junto con las tasas dice qué
  -- monedas tiene que cubrir el grupo, sin depender del resumen.
  }
  deriving (Show, Eq, Generic)

estaCongelado :: ShallowGrupo -> Bool
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
  , isValid :: Bool
  , nombre :: Text
  , fecha :: Day
  , pagadores :: Distribucion
  , deudores :: Distribucion
  }
  deriving (Show, Eq, Generic)

data ShallowPago = ShallowPago
  { pagoId :: ULID
  , isValid :: Bool
  , nombre :: Text
  , monto :: Monto
  , moneda :: Moneda
  , fecha :: Day
  }
  deriving (Show, Eq, Generic)

calcularNetosTotales :: Grupo -> PorMoneda (Netos Monto)
calcularNetosTotales grupo =
  grupo.pagos
    & filter isValid
    & fmap (\pago -> (calcularNetosPago pago) `enMoneda` pago.moneda)
    & mconcat

-- | Los netos que dejan las transacciones ya hechas. Se suman a los de los
-- pagos porque una transferencia hecha es plata que ya se movió, y por eso
-- sobreviven al descongelar: sin ellas un grupo que se congeló, se saldó y se
-- descongeló volvería a mostrar las deudas que ya se pagaron.
netosDeTransacciones :: PorMoneda [Transaccion] -> PorMoneda (Netos Monto)
netosDeTransacciones =
  fmap (foldMap netosDeTransaccion)

calcularNetosPago :: Pago -> Netos Monto
calcularNetosPago pago =
  fromMaybe mempty $ getNetosResumen $ getResumenPago pago

getResumenPago :: Pago -> ResumenNetos
getResumenPago pago =
  let
    resumenPagadores = getResumen pago.monto pago.pagadores
    resumenDeudores = getResumen pago.monto pago.deudores
    netos = resumenPagadores.netos <> fmap negate resumenDeudores.netos
    extraErrors = []
  in
    ResumenNetos pago.monto netos
      $ fmap (relabelError "pagadores") resumenPagadores.errores
      <> fmap (relabelError "deudores") resumenDeudores.errores
      <> extraErrors

isValid :: Pago -> Bool
isValid pago =
  pago
    & getResumenPago
    & getNetosResumen
    & isJust

addIsValidPago :: Pago -> Pago
addIsValidPago pago =
  pago{isValid = isValid pago}

instance IsElmDefinition UTCTime where
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias{epa_name = ETypeName{et_name = "UTCTime", et_args = []}, epa_type = ETyCon (ETCon{tc_name = "String"})})

instance IsElmDefinition Day where
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias{epa_name = ETypeName{et_name = "Day", et_args = []}, epa_type = ETyCon (ETCon{tc_name = "String"})})

Elm.deriveBoth Elm.defaultOptions ''Pago
Elm.deriveBoth Elm.defaultOptions ''ShallowPago
Elm.deriveBoth Elm.defaultOptions ''Grupo
Elm.deriveBoth Elm.defaultOptions ''ShallowGrupo

Elm.deriveBoth Elm.defaultOptions ''GrupoParaUsuario
