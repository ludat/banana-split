{-# LANGUAGE DerivingStrategies #-}

-- | Con cuánto peso entra cada participante en un reparto: partes de una
-- distribución, unidades reclamadas de un item, o lo que ya consumió cada uno
-- cuando lo que se reparte va en esa misma proporción.
--
-- No es plata —por eso no es 'BananaSplit.Netos.Netos'— pero comparte el monoide,
-- porque dos pesos de la misma persona también se suman. Tampoco sale nunca del
-- backend: no tiene instancias de JSON ni de Elm, a diferencia de los netos.
--
-- Lo consume 'BananaSplit.Deudas.distribuirEntrePonderados'. Se usa calificado:
-- @import BananaSplit.Ponderaciones qualified as Ponderaciones@.
module BananaSplit.Ponderaciones (
  aPares,
  desde,
  filtrar,
  Ponderaciones (..),
  ponderar,
  total,
) where

import Protolude

import BananaSplit.Participante (ParticipanteId)
import BananaSplit.PorParticipante (PorParticipante)
import BananaSplit.PorParticipante qualified as PorParticipante

newtype Ponderaciones a = Ponderaciones (PorParticipante a)
  deriving newtype (Show, Eq, Functor, Foldable)

instance (Num a) => Semigroup (Ponderaciones a) where
  Ponderaciones ps1 <> Ponderaciones ps2 = Ponderaciones $ PorParticipante.combinarCon (+) ps1 ps2

instance (Num a) => Monoid (Ponderaciones a) where
  mempty = Ponderaciones $ PorParticipante.desdePares []

ponderar :: ParticipanteId -> a -> Ponderaciones a
ponderar participanteId peso =
  Ponderaciones $ PorParticipante.uno participanteId peso

desde :: PorParticipante a -> Ponderaciones a
desde = Ponderaciones

total :: (Num a) => Ponderaciones a -> a
total = sum

filtrar :: (a -> Bool) -> Ponderaciones a -> Ponderaciones a
filtrar f (Ponderaciones ps) =
  Ponderaciones $ PorParticipante.filtrar f ps

-- | Los pesos de mayor a menor, que es el orden en el que se reparte.
--
-- No es cosmético: el reparto asigna hasta el último centavo recorriendo esta
-- lista, así que el orden decide a quién le toca el redondeo.
aPares :: (Ord a) => Ponderaciones a -> [(ParticipanteId, a)]
aPares (Ponderaciones ps) =
  ps
    & PorParticipante.aPares
    & sortOn (\(p, peso) -> (Down peso, p))
