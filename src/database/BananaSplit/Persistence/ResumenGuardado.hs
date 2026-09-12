{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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
  sinCalcular,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap

import BananaSplit qualified as M
import Preludat

data ResumenGuardado = ResumenGuardado
  { errores :: [M.ErrorResumen]
  , participantesEnRepartija :: Maybe Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.ToJSON)

-- | Lo que dice el resumen de un gasto cuando no lo pudimos leer: no sabemos si
-- cierra. Una lista vacía diría que es válido, que es justo lo que no sabemos.
sinCalcular :: [M.ErrorResumen]
sinCalcular = [M.ErrorResumen{M.objeto = [], M.tipo = M.ErrorNoCalculado}]

-- | Decodifica campo por campo en vez de todo o nada.
--
-- El derivado genérico de Aeson falla entero si un campo no matchea, y eso
-- haría perder los campos que sí se entienden: un cambio de forma de
-- 'M.ErrorResumen' se llevaría puesto el contador de la repartija, que no tiene
-- nada que ver. Acá cada campo que no se puede leer cae por separado a "no
-- sabemos".
--
-- Falla entero sólo si el blob no es un objeto, porque ahí no hay nada que
-- rescatar.
instance Aeson.FromJSON ResumenGuardado where
  parseJSON = Aeson.withObject "ResumenGuardado" $ \o ->
    pure
      ResumenGuardado
        { errores = campoLaxo o "errores" sinCalcular
        , participantesEnRepartija = campoLaxo o "participantesEnRepartija" Nothing
        }

-- | El valor del campo, o el default si falta o no se puede decodificar.
campoLaxo :: (Aeson.FromJSON a) => Aeson.Object -> Aeson.Key -> a -> a
campoLaxo o clave porDefecto =
  case Aeson.KeyMap.lookup clave o of
    Just value
      | Aeson.Success x <- Aeson.fromJSON value -> x
    _ -> porDefecto
