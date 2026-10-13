{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Algo por participante: el mapa pelado, sin ninguna semántica encima.
--
-- Es el mismo molde que 'BananaSplit.Moneda.PorMoneda' pero con participantes de
-- clave, y a propósito no tiene 'Semigroup': juntar dos de estos no quiere decir
-- nada hasta que se sepa qué guardan. Los que sí significan algo son los newtypes
-- de arriba —'BananaSplit.Netos.Netos', 'BananaSplit.Ponderaciones.Ponderaciones'—
-- que derivan de acá lo que no depende de esa decisión (recorrerlo, mapearlo,
-- compararlo) y agregan cada uno la suya.
--
-- Se usa calificado: @import BananaSplit.PorParticipante qualified as PorParticipante@.
module BananaSplit.PorParticipante (
  aPares,
  buscar,
  combinarCon,
  comoMapa,
  desdePares,
  filtrar,
  participantes,
  PorParticipante (..),
  sacar,
  uno,
) where

import Data.Map.Strict qualified as Map
import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.Participante (ParticipanteId)

newtype PorParticipante a = PorParticipante (Map ParticipanteId a)
  deriving newtype (Show, Eq, Functor, Foldable)

uno :: ParticipanteId -> a -> PorParticipante a
uno participanteId a =
  PorParticipante $ Map.singleton participanteId a

desdePares :: [(ParticipanteId, a)] -> PorParticipante a
desdePares pares =
  PorParticipante $ Map.fromList pares

-- | Los pares ordenados por participante. El orden que importa en pantalla o
-- al repartir centavos lo pone cada newtype de arriba, no esto.
aPares :: PorParticipante a -> [(ParticipanteId, a)]
aPares (PorParticipante mapa) =
  Map.toAscList mapa

comoMapa :: PorParticipante a -> Map ParticipanteId a
comoMapa (PorParticipante mapa) = mapa

participantes :: PorParticipante a -> [ParticipanteId]
participantes (PorParticipante mapa) =
  Map.keys mapa

buscar :: ParticipanteId -> PorParticipante a -> Maybe a
buscar participanteId (PorParticipante mapa) =
  Map.lookup participanteId mapa

filtrar :: (a -> Bool) -> PorParticipante a -> PorParticipante a
filtrar f (PorParticipante mapa) =
  PorParticipante $ Map.filter f mapa

sacar :: ParticipanteId -> PorParticipante a -> PorParticipante a
sacar participanteId (PorParticipante mapa) =
  PorParticipante $ Map.delete participanteId mapa

-- | Con qué se juntan los valores de un participante que está en los dos. De
-- acá salen los 'Semigroup' de los newtypes de arriba.
combinarCon :: (a -> a -> a) -> PorParticipante a -> PorParticipante a -> PorParticipante a
combinarCon f (PorParticipante mapa1) (PorParticipante mapa2) =
  PorParticipante $ Map.unionWith f mapa1 mapa2

Elm.deriveBoth Elm.defaultOptions ''PorParticipante
