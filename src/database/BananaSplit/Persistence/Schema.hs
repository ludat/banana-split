{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Persistence.Schema where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (String)
import Data.Time (Day, UTCTime)
import Database.Beam as Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

import BananaSplit qualified as M
import BananaSplit.ULID (ULID)
import Preludat

data BananaSplitDb f = BananaSplitDb
  { grupos :: f (TableEntity GrupoT)
  , users :: f (TableEntity UserT)
  , login_attempts :: f (TableEntity LoginAttemptT)
  , participantes :: f (TableEntity ParticipanteT)
  , pagos :: f (TableEntity PagoT)
  , distribuciones :: f (TableEntity DistribucionT)
  , distribuciones_monto_equitativo :: f (TableEntity DistribucionMontoEquitativoT)
  , distribuciones_monto_equitativo_items :: f (TableEntity DistribucionMontoEquitativoItemT)
  , distribuciones_montos_especificos :: f (TableEntity DistribucionMontosEspecificosT)
  , distribuciones_montos_especificos_items :: f (TableEntity DistribucionMontosEspecificosItemT)
  , distribuciones_partes :: f (TableEntity DistribucionPartesT)
  , distribuciones_partes_items :: f (TableEntity DistribucionPartesItemT)
  , repartijas :: f (TableEntity DistribucionRepartijaT)
  , repartija_items :: f (TableEntity RepartijaItemT)
  , repartija_claims :: f (TableEntity RepartijaClaimT)
  , transacciones_congeladas :: f (TableEntity TransaccionCongeladaT)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings be BananaSplitDb
db = defaultDbSettings

data GrupoT f = Grupo
  { id :: Columnar f ULID
  , nombre :: Columnar f Text
  , is_frozen :: Columnar f Bool
  , moneda_por_defecto :: Columnar f M.Moneda
  }
  deriving (Generic, Beamable)

type GrupoId = PrimaryKey GrupoT Identity

type Grupo = GrupoT Identity

deriving instance Show GrupoId

deriving instance Show Grupo

deriving instance Eq GrupoId

deriving instance Eq Grupo

instance Beam.Table GrupoT where
  data PrimaryKey GrupoT f = GrupoId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = GrupoId . (.id)

data UserT f = User
  { id :: Columnar f ULID
  , email :: Columnar f M.Email
  , nombre :: Columnar f Text
  , created_at :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type UserId = PrimaryKey UserT Identity

type User = UserT Identity

deriving instance Show UserId

deriving instance Eq UserId

deriving instance Show User

deriving instance Eq User

deriving instance Show (PrimaryKey UserT (Nullable Identity))

deriving instance Eq (PrimaryKey UserT (Nullable Identity))

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = UserId . (.id)

data LoginEvent
  = VerifyFailure
  | CodeSent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LoginAttemptT f = LoginAttempt
  { id :: Columnar f ULID
  , email :: Columnar f M.Email
  , details :: Columnar f (PgJSONB LoginEvent)
  , created_at :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type LoginAttempt = LoginAttemptT Identity

deriving instance Show LoginAttempt

deriving instance Eq LoginAttempt

instance Table LoginAttemptT where
  data PrimaryKey LoginAttemptT f = LoginAttemptId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = LoginAttemptId . (.id)

data ParticipanteT f = Participante
  { id :: Columnar f ULID
  , grupo :: PrimaryKey GrupoT f
  , nombre :: Columnar f Text
  , user :: PrimaryKey UserT (Nullable f)
  }
  deriving (Generic, Beamable)

type ParticipanteId = PrimaryKey ParticipanteT Identity

type Participante = ParticipanteT Identity

deriving instance Show ParticipanteId

deriving instance Eq ParticipanteId

deriving instance Show Participante

deriving instance Eq Participante

instance Table ParticipanteT where
  data PrimaryKey ParticipanteT f = ParticipanteId (Columnar f ULID) deriving (Generic, Beamable)
  primaryKey = ParticipanteId . (.id)

data PagoT f = Pago
  { pagoId :: Columnar f ULID
  , pagoIsValid :: Columnar f Bool
  , pagoGrupo :: PrimaryKey GrupoT f
  , pagoNombre :: Columnar f Text
  , pagoMonto :: MontoT f
  , pagoMoneda :: Columnar f M.Moneda
  , distribucion_pagadores :: PrimaryKey DistribucionT f
  , distribucion_deudores :: PrimaryKey DistribucionT f
  , fecha :: Columnar f Day
  }
  deriving (Generic, Beamable)

type PagoId = PrimaryKey PagoT Identity

type Pago = PagoT Identity

deriving instance Show PagoId

deriving instance Eq PagoId

deriving instance Ord PagoId

deriving instance Show Pago

instance Table PagoT where
  newtype PrimaryKey PagoT f = PagoId (Columnar f ULID)
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey = PagoId . (.pagoId)

data MontoT f = Monto
  { montoLugaresDespuesDeLaComa :: Columnar f Int32
  , montoValor :: Columnar f Int32
  }
  deriving (Generic, Beamable)

type Monto = MontoT Identity

deriving instance Show Monto

deriving instance Show (MontoT (Nullable Identity))

deriving instance Eq (MontoT (Nullable Identity))

deriving instance Eq Monto

data DistribucionT f = Distribucion
  { id :: Columnar f ULID
  , tipo :: Columnar f Text
  }
  deriving (Generic, Beamable)

type Distribucion = DistribucionT Identity

deriving instance Show Distribucion

deriving instance Eq Distribucion

type DistribucionId = PrimaryKey DistribucionT Identity

deriving instance Show DistribucionId

deriving instance Eq DistribucionId

instance Table DistribucionT where
  data PrimaryKey DistribucionT f
    = DistribucionId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionId . (.id)

data DistribucionMontosEspecificosT f = DistribucionMontosEspecificos
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  }
  deriving (Generic, Beamable)

type DistribucionMontosEspecifico = DistribucionMontosEspecificosT Identity

deriving instance Show DistribucionMontosEspecifico

deriving instance Eq DistribucionMontosEspecifico

type DistribucionMontosEspecificoId = PrimaryKey DistribucionMontosEspecificosT Identity

deriving instance Show DistribucionMontosEspecificoId

deriving instance Eq DistribucionMontosEspecificoId

instance Table DistribucionMontosEspecificosT where
  data PrimaryKey DistribucionMontosEspecificosT f
    = DistribucionMontosEspecificosId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey distrubucion = DistribucionMontosEspecificosId distrubucion.id

data DistribucionMontosEspecificosItemT f = DistribucionMontosEspecificosItem
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionMontosEspecificosT f
  , participante :: PrimaryKey ParticipanteT f
  , monto :: MontoT f
  }
  deriving (Generic, Beamable)

type DistribucionMontosEspecificosItem = DistribucionMontosEspecificosItemT Identity

deriving instance Show DistribucionMontosEspecificosItem

deriving instance Eq DistribucionMontosEspecificosItem

type DistribucionMontosEspecificosItemId = PrimaryKey DistribucionMontosEspecificosItemT Identity

deriving instance Show DistribucionMontosEspecificosItemId

deriving instance Eq DistribucionMontosEspecificosItemId

instance Table DistribucionMontosEspecificosItemT where
  data PrimaryKey DistribucionMontosEspecificosItemT f
    = DistribucionMontosEspecificosItemId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey distrubucion = DistribucionMontosEspecificosItemId distrubucion.id

data DistribucionPartesT f = DistribucionPartes
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  }
  deriving (Generic, Beamable)

type DistribucionPartes = DistribucionPartesT Identity

deriving instance Show DistribucionPartes

deriving instance Eq DistribucionPartes

type DistribucionPartesId = PrimaryKey DistribucionPartesT Identity

deriving instance Show DistribucionPartesId

deriving instance Eq DistribucionPartesId

instance Table DistribucionPartesT where
  data PrimaryKey DistribucionPartesT f
    = DistribucionPartesId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionPartesId . (.id)

-- | Cada fila es una 'M.Parte'. Según qué columnas estén presentes:
-- solo monto es 'M.MontoFijo', solo cuota es 'M.Ponderado', y ambos es
-- 'M.PonderadoYMontoFijo'. Nunca pueden estar ambos nulos (ver check).
data DistribucionPartesItemT f = DistribucionPartesItem
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionPartesT f
  , participante :: PrimaryKey ParticipanteT f
  , monto :: MontoT (Nullable f)
  , cuota :: C (Nullable f) Int32
  }
  deriving (Generic, Beamable)

type DistribucionPartesItem = DistribucionPartesItemT Identity

deriving instance Show DistribucionPartesItem

deriving instance Eq DistribucionPartesItem

type DistribucionPartesItemId = PrimaryKey DistribucionPartesItemT Identity

deriving instance Show DistribucionPartesItemId

deriving instance Eq DistribucionPartesItemId

instance Table DistribucionPartesItemT where
  data PrimaryKey DistribucionPartesItemT f
    = DistribucionPartesItemId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionPartesItemId . (.id)

data DistribucionMontoEquitativoT f = DistribucionMontoEquitativo
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  }
  deriving (Generic, Beamable)

type DistribucionMontoEquitativo = DistribucionMontoEquitativoT Identity

deriving instance Show DistribucionMontoEquitativo

deriving instance Eq DistribucionMontoEquitativo

type DistribucionMontoEquitativoId = PrimaryKey DistribucionMontoEquitativoT Identity

deriving instance Show DistribucionMontoEquitativoId

deriving instance Eq DistribucionMontoEquitativoId

instance Table DistribucionMontoEquitativoT where
  data PrimaryKey DistribucionMontoEquitativoT f
    = DistribucionMontoEquitativoId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionMontoEquitativoId . (.id)

-- ==============
-- DistribucionMontoEquitativoItem
-- ==============
data DistribucionMontoEquitativoItemT f = DistribucionMontoEquitativoItem
  { distribucion :: PrimaryKey DistribucionMontoEquitativoT f
  , participante :: PrimaryKey ParticipanteT f
  }
  deriving (Generic, Beamable)

type DistribucionMontoEquitativoItem = DistribucionMontoEquitativoItemT Identity

deriving instance Show DistribucionMontoEquitativoItem

deriving instance Eq DistribucionMontoEquitativoItem

type DistribucionMontoEquitativoItemId = PrimaryKey DistribucionMontoEquitativoItemT Identity

deriving instance Show DistribucionMontoEquitativoItemId

deriving instance Eq DistribucionMontoEquitativoItemId

instance Table DistribucionMontoEquitativoItemT where
  data PrimaryKey DistribucionMontoEquitativoItemT f
    = DistribucionMontoEquitativoItemId
        (PrimaryKey DistribucionMontoEquitativoT f)
        (PrimaryKey ParticipanteT f)
    deriving (Generic, Beamable)
  primaryKey = DistribucionMontoEquitativoItemId <$> (.distribucion) <*> (.participante)

data DistribucionRepartijaT f = Repartija
  { id :: Columnar f ULID
  , distribucion :: PrimaryKey DistribucionT f
  , extra :: MontoT f
  , distribucion_de_sobras :: Columnar f Text
  }
  deriving (Generic, Beamable)

type DistribucionRepartijaId = PrimaryKey DistribucionRepartijaT Identity

type DistribucionRepartija = DistribucionRepartijaT Identity

deriving instance Show DistribucionRepartijaId

deriving instance Show DistribucionRepartija

instance Table DistribucionRepartijaT where
  data PrimaryKey DistribucionRepartijaT f = DistribucionRepartijaId (C f ULID)
    deriving (Generic, Beamable)
  primaryKey = DistribucionRepartijaId . (.id)

data RepartijaItemT f = RepartijaItem
  { repartijaitemId :: Columnar f ULID
  , repartijaitemRepartija :: PrimaryKey DistribucionRepartijaT f
  , repartijaitemNombre :: C f Text
  , repartijaitemMonto :: MontoT f
  , repartijaitemCantidad :: C f Int32
  }
  deriving (Generic, Beamable)

type RepartijaItem = RepartijaItemT Identity

type RepartijaItemId = PrimaryKey RepartijaItemT Identity

deriving instance Show RepartijaItem

deriving instance Show RepartijaItemId

instance Table RepartijaItemT where
  data PrimaryKey RepartijaItemT f = RepartijaItemId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaItemId . (.repartijaitemId)

data RepartijaClaimT f = RepartijaClaim
  { repartijaclaimId :: Columnar f ULID
  , repartijaclaimRepartijaItem :: PrimaryKey RepartijaItemT f
  , repartijaclaimParticipante :: PrimaryKey ParticipanteT f
  , repartijaclaimCantidad :: C (Nullable f) Int32
  }
  deriving (Generic, Beamable)

type RepartijaClaim = RepartijaClaimT Identity

type RepartijaClaimId = PrimaryKey RepartijaClaimT Identity

deriving instance Show RepartijaClaim

deriving instance Show RepartijaClaimId

instance Table RepartijaClaimT where
  data PrimaryKey RepartijaClaimT f = RepartijaClaimId (C f ULID) deriving (Generic, Beamable)
  primaryKey = RepartijaClaimId . (.repartijaclaimId)

data TransaccionCongeladaT f = TransaccionCongelada
  { id :: Columnar f ULID
  , grupo :: PrimaryKey GrupoT f
  , participante_from :: PrimaryKey ParticipanteT f
  , participante_to :: PrimaryKey ParticipanteT f
  , monto :: MontoT f
  , moneda :: Columnar f M.Moneda
  }
  deriving (Generic, Beamable)

type TransaccionCongelada = TransaccionCongeladaT Identity

type TransaccionCongeladaId = PrimaryKey TransaccionCongeladaT Identity

deriving instance Show TransaccionCongelada

deriving instance Show TransaccionCongeladaId

instance Table TransaccionCongeladaT where
  data PrimaryKey TransaccionCongeladaT f = TransaccionCongeladaId (Columnar f ULID)
    deriving (Generic, Beamable)
  primaryKey = TransaccionCongeladaId . (.id)

instance HasSqlValueSyntax PgValueSyntax ULID where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres ULID where
  fromBackendRow = read <$> fromBackendRow

instance HasSqlEqualityCheck Postgres ULID

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be M.Moneda where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres M.Moneda where
  fromBackendRow = read <$> fromBackendRow

instance HasSqlEqualityCheck Postgres M.Moneda

instance HasSqlValueSyntax PgValueSyntax M.Email where
  sqlValueSyntax = sqlValueSyntax . M.unEmail

instance FromBackendRow Postgres M.Email where
  fromBackendRow = M.mkEmail <$> fromBackendRow

instance HasSqlEqualityCheck Postgres M.Email
