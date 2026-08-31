module BananaSplit.TestUtils (
  distribucionMontoEquitativo,
  distribucionMontosEspecificos,
  distribucionPartes,
  distribucionRepartija,
  fakeUlid,
  netos,
  participante,
  getNetos,
  fatalError,
  tasaEntre,
) where

import Data.Maybe (fromJust)
import Protolude

import BananaSplit

fatalError :: Text -> FatalError -> Bool
fatalError msg e = fatalErrorMessage e == msg

participante :: Integer -> ParticipanteId
participante = ParticipanteId . fakeUlid

-- | Una tasa sin id: @tasaEntre USD 1 ARS 1000@ es "1 USD son 1000 ARS", que
-- es lo mismo que @tasaEntre ARS 1000 USD 1@.
tasaEntre :: Moneda -> Monto -> Moneda -> Monto -> TasaDeCambio
tasaEntre una monto otra otroDeLosMontos =
  TasaDeCambio
    { id = nullUlid
    , unaMoneda = una
    , otraMoneda = otra
    , unMonto = monto
    , otroMonto = otroDeLosMontos
    }

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

-- | Montos específicos modelados como partes con montos fijos.
distribucionMontosEspecificos :: [(ParticipanteId, Monto)] -> Distribucion
distribucionMontosEspecificos ps =
  Distribucion (fakeUlid 21) $
    TipoDistribucionPartes $
      DistribucionPartes
        (fakeUlid 12)
        ((\(p, m) -> MontoFijo m p) <$> ps)

-- | Monto equitativo modelado como partes ponderadas con 1 cada una.
distribucionMontoEquitativo :: [ParticipanteId] -> Distribucion
distribucionMontoEquitativo ps =
  Distribucion (fakeUlid 21) $
    TipoDistribucionPartes $
      DistribucionPartes (fakeUlid 12) (Ponderado 1 <$> ps)

distribucionPartes :: [Parte] -> Distribucion
distribucionPartes ps = Distribucion (fakeUlid 21) $ TipoDistribucionPartes $ DistribucionPartes (fakeUlid 12) ps

distribucionRepartija :: Repartija -> Distribucion
distribucionRepartija r = Distribucion (fakeUlid 21) $ TipoDistribucionRepartija r
