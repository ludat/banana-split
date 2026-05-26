module BananaSplit.TestUtils (
  distribucionMontoEquitativo,
  distribucionMontosEspecificos,
  distribucionRepartija,
  fakeUlid,
  netos,
  participante,
  getNetos,
  fatalError,
) where

import Data.Maybe (fromJust)
import Protolude

import BananaSplit

fatalError :: Text -> FatalError -> Bool
fatalError msg e = fatalErrorMessage e == msg

participante :: Integer -> ParticipanteId
participante = ParticipanteId . fakeUlid

fakeUlid :: Integer -> ULID
fakeUlid integer =
  case ulidFromInteger integer of
    Right ulid -> ulid
    Left e -> panic e

getNetos :: (HasResumen a) => Monto -> a -> Netos Monto
getNetos totalPago = fromJust . getNetosResumen . getResumen totalPago

netos :: [(ParticipanteId, Monto)] -> Netos Monto
netos l =
  l
    & fmap (uncurry mkDeuda)
    & mconcat

distribucionMontosEspecificos :: [(ParticipanteId, Monto)] -> Distribucion
distribucionMontosEspecificos ps =
  Distribucion (fakeUlid 21) $
    TipoDistribucionMontosEspecificos $
      DistribucionMontosEspecificos
        (fakeUlid 12)
        ((\(n, (p, m)) -> MontoEspecifico{id = fakeUlid n, monto = m, participante = p}) <$> zip [0 ..] ps)

distribucionMontoEquitativo :: [ParticipanteId] -> Distribucion
distribucionMontoEquitativo ps = Distribucion (fakeUlid 21) $ TipoDistribucionMontoEquitativo $ DistribucionMontoEquitativo (fakeUlid 12) ps

distribucionRepartija :: Repartija -> Distribucion
distribucionRepartija r = Distribucion (fakeUlid 21) $ TipoDistribucionRepartija r
