{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Solver where

import BananaSplit.Core

import Data.LinearProgram qualified as GLPK
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Elm.Derive qualified as Elm

import Money qualified

import Protolude

import System.IO.Unsafe (unsafePerformIO)

minimizeTransactions :: Deudas Monto -> [Transaccion]
minimizeTransactions = solveOptimalTransactions

resolverDeudasRecursivo :: Deudas Monto -> [Transaccion]
resolverDeudasRecursivo netBalances =
  let allValidSettlements = settleDebts $ deudasToPairs netBalances
  in allValidSettlements
    & \case
        [] -> []
        _ -> minimumBy (comparing length) allValidSettlements
  where
    settleDebts :: [(ParticipanteId, Monto)] -> [[Transaccion]]
    settleDebts [] = [[]]
    settleDebts [_] = [[]]
    settleDebts ((personOwing, balance):others)
      | balance == 0 = settleDebts others
      | otherwise = concatMap attemptSettlement possiblePartners
      where
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

data Transaccion = Transaccion
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

-- Main function to solve the debt problem
solveOptimalTransactions :: Deudas Monto -> [Transaccion]
solveOptimalTransactions deudas@(Deudas oldBalances) = unsafePerformIO $ do
    -- 1. Preprocessing and Validation
    let
      balances :: Map ParticipanteId Double
      balances = fmap realToFrac oldBalances
    let balanceSum = sum $ Map.elems balances
    let debtorMap   = Map.filter (< 0) balances
    let creditorMap = Map.filter (> 0) balances
    let debtors     = Map.keys debtorMap
    let creditors   = Map.keys creditorMap

    -- If no debts, no transactions needed
    if Map.null debtorMap then pure [] else do

        -- A "big M" value, larger than any possible transaction
        let bigM = sum (abs <$> Map.elems balances)

        -- Create mappings from (debtor, creditor) pairs to column indices
        let d_c_pairs = [(d, c) | d <- debtors, c <- creditors]

        r <- GLPK.glpSolveVars GLPK.mipDefaults{GLPK.msgLev = GLPK.MsgErr} $ GLPK.execLPM @Text @Double $ do
          GLPK.setDirection GLPK.Min

          -- Set the objective for the existing keys
          d_c_pairs
            & fmap (\(fromP, toP) -> (1, "e_" <> show fromP <> "_" <> show toP))
            & GLPK.linCombination
            & GLPK.setObjective

          -- Make e_ variables binary
          d_c_pairs
            & fmap (\(fromP, toP) -> "e_" <> show fromP <> "_" <> show toP)
            & mapM_ (`GLPK.setVarKind` GLPK.BinVar)

          -- All transactions should be positive
          d_c_pairs
            & fmap (\(fromP, toP) -> "t_" <> show fromP <> "_" <> show toP)
            & mapM_ (`GLPK.varGeq` 0)

          forM_ debtors $ \debtor -> do
            creditors
            & fmap (\c -> (1, "t_" <> show debtor <> "_" <> show c))
            & GLPK.linCombination
            & (\f -> GLPK.constrain' ("Debtor: " <> show debtor) f $ between (negate $ balances Map.! debtor) balanceSum)
          forM_ creditors $ \creditor -> do
            debtors
            & fmap (\d -> (1, "t_" <> show d <> "_" <> show creditor))
            & GLPK.linCombination
            & (\f -> GLPK.equalTo' ("Creditor: " <> show creditor) f (balances Map.! creditor))
            -- & (\f -> GLPK.constrain' ("Creditor: " <> show creditor) f $ between (balances Map.! creditor) balanceSum)

          -- case compare balanceSum 0 of
          --   EQ -> do
          --     forM_ debtors $ \debtor -> do
          --       creditors
          --       & fmap (\c -> (1, "t_" <> show debtor <> "_" <> show c))
          --       & GLPK.linCombination
          --       & (\f -> GLPK.equalTo' ("Debtor: " <> show debtor) f (negate $ balances Map.! debtor))
          --     forM_ creditors $ \creditor -> do
          --       debtors
          --       & fmap (\d -> (1, "t_" <> show d <> "_" <> show creditor))
          --       & GLPK.linCombination
          --       & (\f -> GLPK.equalTo' ("Creditor: " <> show creditor) f (balances Map.! creditor))
          --   LT -> do
          --     forM_ debtors $ \debtor -> do
          --       creditors
          --       & fmap (\c -> (1, "t_" <> show debtor <> "_" <> show c))
          --       & GLPK.linCombination
          --       & (\f -> GLPK.equalTo' ("Debtor: " <> show debtor) f (negate $ balances Map.! debtor))
          --     forM_ creditors $ \creditor -> do
          --       debtors
          --       & fmap (\d -> (1, "t_" <> show d <> "_" <> show creditor))
          --       & GLPK.linCombination
          --       & (\f -> GLPK.geqTo' ("Creditor: " <> show creditor) f (balances Map.! creditor))
          --   GT -> do
          --     forM_ debtors $ \debtor -> do
          --       creditors
          --       & fmap (\c -> (1, "t_" <> show debtor <> "_" <> show c))
          --       & GLPK.linCombination
          --       & (\f -> GLPK.geqTo' ("Debtor: " <> show debtor) f (negate $ balances Map.! debtor))
          --     forM_ creditors $ \creditor -> do
          --       debtors
          --       & fmap (\d -> (1, "t_" <> show d <> "_" <> show creditor))
          --       & GLPK.linCombination
          --       & (\f -> GLPK.equalTo' ("Creditor: " <> show creditor) f (balances Map.! creditor))

          forM_ creditors $ \creditor -> do
            forM_ debtors $ \debtor -> do
              GLPK.leq' ("Binary var: " <> show debtor <> " and " <> show creditor)
                (GLPK.linCombination [(1, "t_" <> show debtor <> "_" <> show creditor)])
                (GLPK.linCombination [(bigM, "e_" <> show debtor <> "_" <> show creditor)])
        case r of
          (GLPK.Success, Just (_, m)) -> do
            d_c_pairs
              & mapMaybe (\(d, c) ->
                m
                  & Map.lookup ("t_" <> show d <> "_" <> show c)
                  & mfilter (/= 0)
                  & fmap (\v -> Transaccion
                    { transaccionFrom = d
                    , transaccionTo = c
                    , transaccionMonto = Monto $ Money.dense' $ realToFrac v
                    })
                  )
              & pure
          _ -> do
            pure $ resolverDeudasNaif deudas

between a delta =
  case compare delta 0 of
    EQ -> GLPK.Equ a
    LT -> GLPK.Bound (a + delta) a
    GT -> GLPK.Bound a (a + delta)

Elm.deriveBoth Elm.defaultOptions ''Transaccion
Elm.deriveBoth Elm.defaultOptions ''Deudas
