{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- | El jsonb donde se guarda la parte del resumen de un gasto que no son
-- números por participante, y que por eso no vive en @pago_netos@: no se puede
-- sumar en SQL.
--
-- Es un detalle de cómo se guarda y nada más. Al resto de la app le llega un
-- 'BananaSplit.Core.ResumenGasto' armado, y no tiene por qué saber que una
-- mitad salió de un blob y la otra de una tabla. Vive en su propio módulo para
-- poder testear su decoder sin exponerlo en la API de
-- 'BananaSplit.Persistence'.
module BananaSplit.Persistence.ResumenGuardado (
  ResumenGuardado (..),
  erroresDeErroresFaltantes,
  resumen2Guardado,
  esValido_,
) where

import Data.Aeson
import Data.Coerce (coerce)
import Database.Beam ((==.))
import Database.Beam qualified as Beam
import Database.Beam.Backend.SQL (HasSqlValueSyntax)
import Database.Beam.Backend.SQL.Row (FromBackendRow)
import Database.Beam.Postgres (PgJSONB (..), Postgres, (->$))
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Query.Internal (QGenExpr)

import BananaSplit qualified as M
import Preludat

data ResumenGuardado = ResumenGuardado
  { errores :: [M.ErrorResumen]
  , participantesEnRepartija :: Maybe Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)
  deriving (HasSqlValueSyntax PgValueSyntax, FromBackendRow Postgres) via (PgJSONB ResumenGuardado)

erroresDeErroresFaltantes :: [M.ErrorResumen]
erroresDeErroresFaltantes = [M.ErrorResumen{M.objeto = [], M.tipo = M.ErrorNoCalculado}]

instance FromJSON ResumenGuardado where
  parseJSON = withObject "ResumenGuardado" $ \o ->
    ResumenGuardado
      <$> optional (o .: "errores")
      .!= erroresDeErroresFaltantes
      <*> optional (o .: "participantesEnRepartija")

resumen2Guardado :: M.ResumenGasto -> ResumenGuardado
resumen2Guardado resumen =
  ResumenGuardado
    { errores = resumen.errores
    , participantesEnRepartija = resumen.participantesEnRepartija
    }

esValido_ ::
  QGenExpr ctxt Postgres s ResumenGuardado
  -> QGenExpr ctxt Postgres s Bool
esValido_ resumen =
  (comoJson resumen ->$ Beam.val_ "errores") ==. Beam.val_ (PgJSONB (Array mempty))
  where
    comoJson :: QGenExpr ctxt Postgres s a -> QGenExpr ctxt Postgres s (PgJSONB a)
    comoJson = coerce
