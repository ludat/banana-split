{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Cuánta plata le toca a cada participante.
--
-- Es un 'PorParticipante' con un monoide: juntar dos netos suma lo de cada uno.
-- Esa es la decisión del dominio que el genérico no toma —dos aportes de la misma
-- persona se suman— y la que deja escribir con 'foldMap' casi todo
-- 'BananaSplit.Deudas'.
--
-- Vive en su propio módulo —y no en 'BananaSplit.Deudas', de donde salió— porque
-- 'BananaSplit.Repartija' lo necesita para el desglose que se le manda al
-- frontend, y Deudas depende de Repartija. Deudas lo re-exporta, así que para el
-- resto del código no se movió nada.
module BananaSplit.Netos (
  comoPorParticipante,
  deudasToPairs,
  deudoresNoNulos,
  filterNetos,
  mkDeuda,
  Netos (..),
  totalNetos,
) where

import Elm.Derive qualified as Elm
import Protolude

import BananaSplit.Monto (Monto)
import BananaSplit.Participante (ParticipanteId)
import BananaSplit.PorParticipante (PorParticipante)
import BananaSplit.PorParticipante qualified as PorParticipante

newtype Netos a = Netos (PorParticipante a)
  deriving newtype (Show, Eq, Functor, Foldable)

instance (Num a) => Semigroup (Netos a) where
  Netos ps1 <> Netos ps2 = Netos $ PorParticipante.combinarCon (+) ps1 ps2

instance (Num a) => Monoid (Netos a) where
  mempty = Netos $ PorParticipante.desdePares []

mkDeuda :: ParticipanteId -> a -> Netos a
mkDeuda participanteId monto =
  Netos $ PorParticipante.uno participanteId monto

totalNetos :: (Num a) => Netos a -> a
totalNetos = sum

filterNetos :: (a -> Bool) -> Netos a -> Netos a
filterNetos f (Netos ps) =
  Netos $ PorParticipante.filtrar f ps

deudoresNoNulos :: Netos Monto -> Int
deudoresNoNulos =
  length . filterNetos (/= 0)

-- | Los mismos netos como lista, del que más pone al que más se lleva: el
-- orden en que se muestran y en el que los recorren los algoritmos de deudas.
deudasToPairs :: (Ord a) => Netos a -> [(ParticipanteId, a)]
deudasToPairs (Netos ps) =
  ps
    & PorParticipante.aPares
    & sortOn (\(p, m) -> (Down m, p))

-- | Sacarles la semántica, para volver a ponerles otra: lo que necesita quien
-- usa unos netos como peso de un reparto, por ejemplo.
comoPorParticipante :: Netos a -> PorParticipante a
comoPorParticipante (Netos ps) = ps

Elm.deriveBoth Elm.defaultOptions ''Netos
