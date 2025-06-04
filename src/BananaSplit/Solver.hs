{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
module BananaSplit.Solver where

import BananaSplit.Core

import Data.List qualified as List
import Data.Map qualified as Map

import Elm.Derive qualified as Elm

import Money qualified

import Protolude

minimizeTransactions :: Deudas Monto -> [Transaccion]
minimizeTransactions = resolverDeudasNaif

-- optimizarTransacciones :: [Transaccion] -> [Transaccion]
-- optimizarTransacciones [] = []
-- optimizarTransacciones oldTransacciones =
--   let
--     participantes :: [ParticipanteId]
--     participantes =
--       oldTransacciones
--       & concatMap (\(Transaccion from to _) -> [from, to])
--       & sort
--       & nub

--     cantidadDeParticipantes =
--       length participantes

--     indexFromParticipante :: HasCallStack => ParticipanteId -> Int
--     indexFromParticipante p =
--       participantes
--       & elemIndex p
--       & fromMaybe (error $ "participante not found: " ++ show p)

--     coso =
--       oldTransacciones
--       & fmap (\(Transaccion from to monto) -> (
--         ( indexFromParticipante to
--         , indexFromParticipante from
--         )
--         , fromRational $ toRational $ fst $ Money.discreteFromDense @_ @(Money.UnitScale "ARS" "peso") Money.Floor $ monto2Dense monto))

--     deudasMatrix :: Matrix R
--     deudasMatrix =
--       assoc (cantidadDeParticipantes, cantidadDeParticipantes) 0
--         coso
--     constraints =
--       participantes
--       & zip [0..]
--       & fmap (\(index, _) -> dothething (forpersona index cantidadDeParticipantes) :==: sumElements (deudasMatrix * forpersona index cantidadDeParticipantes))
--       & (++ [dothething (ident cantidadDeParticipantes) :==: 0])

--     result = exact (Minimize (dothething deudasMatrix)) (Dense constraints) []


--   in case result of
--     Undefined -> undefined
--     Feasible _ -> undefined
--     Infeasible _ -> undefined
--     NoFeasible -> undefined
--     Unbounded-> undefined
--     Optimal (_, matrizDeDeudasOptima) ->
--       let
--         nuevaMatriz :: Matrix R
--         nuevaMatriz = traceShowId $ reshape cantidadDeParticipantes $ fromList matrizDeDeudasOptima
--         nuevasTransacciones =
--           [(toIndex, fromIndex) | toIndex <- [0..cantidadDeParticipantes - 1], fromIndex <- [0..cantidadDeParticipantes - 1] ]
--           & fmap (\(toIndex, fromIndex) ->
--             Transaccion
--               (participantes !! toIndex)
--               (participantes !! fromIndex)
--               (Monto $ Money.dense' $ toRational $ nuevaMatriz `atIndex` (fromIndex, toIndex))
--               )
--           & filter (\(Transaccion _ _ monto) -> monto /= 0)
--           & sortOn (\(Transaccion _ _ monto) -> Down monto)

--       in nuevasTransacciones

-- dothething :: Matrix R -> [Double]
-- dothething oldMatrix =
--   toList $ flatten oldMatrix

-- forpersona :: Int -> Int -> Matrix R
-- forpersona index n =
--   let queLeDeben = [((index, i), -1) | i <- [0..n - 1]]
--       queMeDeben = [((i, index), 1) | i <- [0..n - 1]]
--   in assoc (n,n) 0 queMeDeben + assoc (n,n) 0 queLeDeben

settleDebts :: [(ParticipanteId, Monto)] -> [[Transaccion]]
settleDebts [] = [[]]
settleDebts [_] = [[]]
settleDebts ((personOwing, balance):others)
  | balance == 0 = settleDebts others
  | otherwise = concatMap attemptSettlement possiblePartners
  where
    -- Find people with balances of opposite sign
    possiblePartners = [(partner, partnerBalance) | (partner, partnerBalance) <- others, balance * partnerBalance < 0]

    attemptSettlement :: (ParticipanteId, Monto) -> [[Transaccion]]
    attemptSettlement (partner, partnerBalance) =
      let paymentAmount = min (abs balance) (abs partnerBalance)
          newBalanceOwing = balance + signum partnerBalance * paymentAmount
          newPartnerBalance = partnerBalance + signum balance * paymentAmount

          updatedOthers =
            insertOrRemove (partner, newPartnerBalance) (List.delete (partner, partnerBalance) others)

          nextBalances =
            if newBalanceOwing == 0
              then updatedOthers
              else (personOwing, newBalanceOwing) : updatedOthers

          transaction =
            if balance > 0
              then Transaccion partner personOwing paymentAmount
              else Transaccion personOwing partner paymentAmount
      in fmap (transaction :) (settleDebts nextBalances)

-- Helper to insert updated balances (or remove if settled)
insertOrRemove :: (ParticipanteId, Monto) -> [(ParticipanteId, Monto)] -> [(ParticipanteId, Monto)]
insertOrRemove (_, 0) balances = balances
insertOrRemove updatedBalance balances = updatedBalance : deleteByPerson (fst updatedBalance) balances

deleteByPerson :: ParticipanteId -> [(ParticipanteId, Monto)] -> [(ParticipanteId, Monto)]
deleteByPerson name = filter ((/= name) . fst)

-- Entry point: find the minimal set of transactions to settle debts
resolverDeudasRecursivo :: Deudas Monto -> [Transaccion]
resolverDeudasRecursivo netBalances =
  let allValidSettlements = settleDebts $ deudasToPairs netBalances
  in allValidSettlements
    & \case
        [] -> []
        _ -> minimumBy (comparing length) allValidSettlements

data Transaccion =
  Transaccion
  { transaccionFrom :: ParticipanteId
  , transaccionTo :: ParticipanteId
  , transaccionMonto :: Monto
  } deriving (Show, Eq, Generic)


deudoresNoNulos :: Deudas Monto -> Int
deudoresNoNulos (Deudas deudasMap) =
  deudasMap
  & Map.filter (/= 0)
  & length

filterDeudas :: (a -> Bool) -> Deudas a -> Deudas a
filterDeudas f (Deudas deudasMap) =
  deudasMap
  & Map.filter f
  & Deudas

newtype Deudas a = Deudas (Map ParticipanteId a)
  deriving newtype (Show, Eq, Functor)

deudasToPairs :: Ord a => Deudas a -> [(ParticipanteId, a)]
deudasToPairs (Deudas deudasMap) =
  deudasMap
  & Map.toAscList
  & sortOn (\(p, m) -> (Down m, p))

instance Num a => Monoid (Deudas a) where
  mempty = Deudas Map.empty

instance Num a => Semigroup (Deudas a) where
  Deudas d1 <> Deudas d2 = Deudas $ Map.unionWith (+) d1 d2

totalDeudas :: Num a => Deudas a -> a
totalDeudas (Deudas deudasMap) =
  deudasMap
  & Map.elems
  & sum

calcularDeudasTotales :: Grupo -> Deudas Monto
calcularDeudasTotales grupo =
  grupo.pagos
  & fmap calcularDeudasPago
  & mconcat

mkDeuda :: ParticipanteId -> a -> Deudas a
mkDeuda participanteId monto =
  Deudas $ Map.singleton participanteId monto

calcularDeudasPago :: Pago -> Deudas Monto
calcularDeudasPago pago =
  calcularDeudas pago.monto pago.pagadores <> fmap (* -1) (calcularDeudas pago.monto pago.deudores)
  where
    calcularDeudas montoOriginal partes =
      let
        (ponderados, fijos) =
          partes
          & fmap (\case
            Ponderado parte participante -> Left (participante, parte)
            MontoFijo monto participante -> Right (participante, monto)
          )
          & partitionEithers
        deudasFijos =
          fijos
          & fmap (uncurry mkDeuda)
          & mconcat
        totalFijo =
          totalDeudas deudasFijos
        deudasPonderados =
          ponderados
          & fmap (uncurry mkDeuda)
          & mconcat
      in deudasFijos <> distribuirEntrePonderados (montoOriginal - totalFijo) deudasPonderados

distribuirEntrePonderados :: (Real n) => Monto -> Deudas n -> Deudas Monto
distribuirEntrePonderados (Monto m) deudas =
  let
    parteTotal = totalDeudas deudas
  in deudas
      & fmap (\p -> Monto $ Money.dense' $ toRational p * toRational m / toRational parteTotal)

extraerMaximoDeudor :: Deudas Monto -> (ParticipanteId, Monto)
extraerMaximoDeudor (Deudas deudasMap) =
  deudasMap
  & Map.filter (< 0)
  & fmap (* -1)
  & Map.toList
  & List.maximumBy (compare `on` snd)

extraerMaximoPagador :: Deudas Monto -> (ParticipanteId, Monto)
extraerMaximoPagador (Deudas deudasMap) =
  deudasMap
  & Map.filter (> 0)
  & Map.toList
  & List.maximumBy (compare `on` snd)

extraerDeudor :: Num a => ParticipanteId -> Deudas a -> (a, Deudas a)
extraerDeudor unId (Deudas deudasMap) =
  ( deudasMap & Map.findWithDefault 0 unId
  , Deudas $ deudasMap & Map.delete unId)

removerDeudor :: ParticipanteId -> Deudas m -> Deudas m
removerDeudor participanteId (Deudas deudasMap) =
  Deudas $ Map.delete participanteId deudasMap

resolverDeudasNaif :: Deudas Monto -> [Transaccion]
resolverDeudasNaif deudas
  | deudoresNoNulos deudas == 0 = []
  | deudoresNoNulos deudas == 1 = [] -- error $ show deudas
  | otherwise =
      let
        (mayorDeudor, mayorDeuda) = extraerMaximoDeudor deudas
        deudas' = removerDeudor mayorDeudor deudas
        (mayorPagador, mayorPagado) = extraerMaximoPagador deudas'
        deudas'' = removerDeudor mayorPagador deudas'
      in case compare mayorDeuda mayorPagado of
          LT -> Transaccion mayorDeudor mayorPagador mayorDeuda
            : resolverDeudasNaif (deudas'' <> mkDeuda mayorPagador (mayorPagado - mayorDeuda))
          GT -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverDeudasNaif (deudas'' <> mkDeuda mayorDeudor (-mayorDeuda + mayorPagado))
          EQ -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverDeudasNaif deudas''

Elm.deriveBoth Elm.defaultOptions ''Transaccion
Elm.deriveBoth Elm.defaultOptions ''Deudas