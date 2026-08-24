-- | Consolidación de netos en varias monedas a una sola moneda destino,
-- usando cotizaciones provistas por el usuario.
module BananaSplit.Consolidacion (
  ErrorConsolidacion (..),
  consolidarNetos,
) where

import Data.Decimal qualified as Decimal
import Data.Map.Strict qualified as Map
import Protolude

import BananaSplit.Deudas
import BananaSplit.Moneda
import BananaSplit.Monto

data ErrorConsolidacion
  = -- | Hay netos en esta moneda pero no se dio una cotización.
    CotizacionFaltante Moneda
  | -- | La cotización para esta moneda es cero o negativa.
    CotizacionInvalida Moneda
  deriving (Show, Eq, Generic)

-- | Convertir los netos de cada moneda a la moneda destino y sumarlos.
--
-- Cada cotización expresa cuánto vale 1 unidad de esa moneda en la moneda
-- destino. La moneda destino no necesita cotización (vale 1) y se ignora si
-- viene incluida. Los netos convertidos se redondean a 2 decimales y el
-- residuo de redondeo se descuenta del neto de mayor valor absoluto para
-- garantizar que el total siga siendo cero (invariante que
-- 'minimizeTransactions' requiere).
consolidarNetos ::
  Moneda ->
  PorMoneda Monto ->
  PorMoneda (Netos Monto) ->
  Either ErrorConsolidacion (Netos Monto)
consolidarNetos monedaDestino (PorMoneda cotizaciones) (PorMoneda netosPorMoneda) = do
  traverse_ validarCotizacion $ Map.toList cotizaciones
  netosConvertidos <- Map.traverseWithKey convertir netosPorMoneda
  pure $ ajustarResiduo $ mconcat $ Map.elems netosConvertidos
  where
    validarCotizacion :: (Moneda, Monto) -> Either ErrorConsolidacion ()
    validarCotizacion (moneda, cotizacion) =
      when (cotizacion <= 0) $ Left $ CotizacionInvalida moneda

    convertir :: Moneda -> Netos Monto -> Either ErrorConsolidacion (Netos Monto)
    convertir moneda netos
      | moneda == monedaDestino = pure netos
      | otherwise =
          case Map.lookup moneda cotizaciones of
            Nothing -> Left $ CotizacionFaltante moneda
            Just (Monto cotizacion) ->
              pure $ fmap (\(Monto neto) -> Monto $ Decimal.roundTo 2 $ neto * cotizacion) netos

-- | Si el total no da cero (por redondeo), descontar la diferencia del neto
-- de mayor valor absoluto (a igual valor, el de menor ParticipanteId).
ajustarResiduo :: Netos Monto -> Netos Monto
ajustarResiduo netos@(Netos netosMap) =
  case totalNetos netos of
    0 -> netos
    residuo ->
      case sortOn (\(p, m) -> (Down (abs m), p)) (Map.toList netosMap) of
        [] -> netos
        (mayorParticipante, _) : _ ->
          Netos $ Map.adjust (subtract residuo) mayorParticipante netosMap
