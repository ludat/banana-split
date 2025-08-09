module BananaSplit.TestUtils
    ( deudas
    , distribucionMontoEquitativo
    , distribucionMontosEspecificos
    , fakeUlid
    , participante
    ) where

import BananaSplit

import Protolude
import Protolude.Error

participante :: Integer -> ParticipanteId
participante = ParticipanteId . fakeUlid

fakeUlid :: Integer -> ULID
fakeUlid integer =
  case ulidFromInteger integer of
    Right ulid -> ulid
    Left e -> error e

deudas :: [(ParticipanteId, Monto)] -> Deudas Monto
deudas l =
  l
  & fmap (uncurry mkDeuda)
  & mconcat

distribucionMontosEspecificos :: [(ParticipanteId, Monto)] -> Distribucion
distribucionMontosEspecificos ps = Distribucion (fakeUlid 21) $ TipoDistribucionMontosEspecificos $ DistribucionMontosEspecificos (fakeUlid 12) ps

distribucionMontoEquitativo :: [ParticipanteId] -> Distribucion
distribucionMontoEquitativo ps = Distribucion (fakeUlid 21) $ TipoDistribucionMontoEquitativo $ DistribucionMontoEquitativo (fakeUlid 12) ps
