{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Deudas where

import BananaSplit.Monto
import BananaSplit.Monto qualified as Monto
import BananaSplit.Participante
import BananaSplit.Repartija
import BananaSplit.ULID

import Data.Decimal (Decimal)
import Data.Decimal qualified as Decimal
import Data.LinearProgram qualified as GLPK
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Elm.Derive qualified as Elm

import Protolude
import Protolude.Error (error)

import System.IO.Unsafe (unsafePerformIO)


data Distribucion = Distribucion
  { id :: ULID
  , tipo :: TipoDistribucion
  } deriving (Show, Eq, Generic)

data TipoDistribucion
  = TipoDistribucionMontosEspecificos DistribucionMontosEspecificos
  | TipoDistribucionMontoEquitativo DistribucionMontoEquitativo
  | TipoDistribucionRepartija Repartija
  deriving (Show, Eq, Generic)

data DistribucionMontosEspecificos = DistribucionMontosEspecificos
  { id :: ULID
  , montos :: [MontoEspecifico]
  } deriving (Show, Eq, Generic)

data MontoEspecifico = MontoEspecifico
  { id :: ULID
  , participante :: ParticipanteId
  , monto :: Monto
  } deriving (Show, Eq, Generic)

data DistribucionMontoEquitativo = DistribucionMontoEquitativo
  { id :: ULID
  , participantes :: [ParticipanteId]
  } deriving (Show, Eq, Generic)

data ResumenDeudas
  = DeudasIncomputables (Maybe Monto) ErrorResumen
  | ResumenDeudas (Maybe Monto) (Deudas Monto)
  deriving (Show, Eq, Generic)

data ErrorResumen
  = ErrorResumen (Maybe Text) [(Text, ErrorResumen)]
  deriving (Show, Eq, Generic)

instance Monoid ErrorResumen where
  mempty = ErrorResumen mempty []

instance Semigroup ErrorResumen where
  ErrorResumen msg1 errs1 <> ErrorResumen msg2 errs2 =
    ErrorResumen (msg1 <> msg2) (errs1 <> errs2)

getDeudas :: HasResumen a => Monto -> a -> Maybe (Deudas Monto)
getDeudas totalPago = getDeudasResumen . getResumen totalPago

getDeudasResumen :: ResumenDeudas -> Maybe (Deudas Monto)
getDeudasResumen resumen =
  case resumen of
    DeudasIncomputables _ _ -> Nothing
    ResumenDeudas _ deudas -> Just deudas

class HasResumen a where
  getResumen :: Monto -> a -> ResumenDeudas

instance HasResumen ResumenDeudas where
  getResumen _ = identity

instance HasResumen Distribucion where
  getResumen totalPago distribucion =
    case distribucion.tipo of
      TipoDistribucionMontoEquitativo d -> getResumen totalPago d
      TipoDistribucionMontosEspecificos d -> getResumen totalPago d
      TipoDistribucionRepartija r -> getResumen totalPago r

instance HasResumen DistribucionMontosEspecificos where
  getResumen totalPago distribucion =
    if | null distribucion.montos -> DeudasIncomputables (Just 0) $ ErrorResumen (Just "No hay montos especificados") []
       | total /= totalPago -> DeudasIncomputables (Just total) $ ErrorResumen (Just $ "El total debería ser igual al total del pago pero es: " <> show total <> " en vez de " <> show totalPago) []
       | otherwise -> ResumenDeudas (Just total) deudas
    where
      deudas = distribucion.montos <&> (\m -> mkDeuda m.participante m.monto) & mconcat
      total = totalDeudas deudas

instance HasResumen DistribucionMontoEquitativo where
  getResumen totalPago d =
    if | null d.participantes -> DeudasIncomputables (Just totalPago) $ ErrorResumen (Just "No hay participantes especificados") []
       | otherwise -> ResumenDeudas (Just totalPago) $ calcularDeudasMontoEquitativo totalPago d

instance HasResumen Repartija where
  getResumen totalPago repartija =
    if | null repartija.items -> DeudasIncomputables (Just totalPorItems) $ ErrorResumen (Just "No hay items para repartir.") []
       | totalPorItems /= totalPago -> DeudasIncomputables (Just totalPorItems) $ ErrorResumen (Just $ "El total de items debería ser igual al total del pago pero es: " <> show totalPorItems <> " en vez de " <> show totalPago) []
       | null repartija.claims -> DeudasIncomputables (Just totalPorItems) $ ErrorResumen (Just "Nadie reclamo ningun item.") []
       | totalPorDeudas /= totalPago -> DeudasIncomputables (Just totalPorItems) $ ErrorResumen (Just $ "El total de deudas debería ser igual al total del pago pero es: " <> show totalPorDeudas <> " en vez de " <> show totalPago) []
       | otherwise ->
           ResumenDeudas (Just totalPorItems) deudas
    where
      totalPorItems = totalRepartija repartija
      deudas = calcularDeudasRepartija repartija
      totalPorDeudas = totalDeudas deudas

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

mkDeuda :: ParticipanteId -> a -> Deudas a
mkDeuda participanteId monto =
  Deudas $ Map.singleton participanteId monto

calcularDeudasMontoEquitativo :: Monto -> DistribucionMontoEquitativo -> Deudas Monto
calcularDeudasMontoEquitativo total distribucion =
  distribucion.participantes
  & fmap (`mkDeuda` 1)
  & mconcat
  & distribuirEntrePonderados total

distribuirEntrePonderados :: Monto -> Deudas Decimal -> Deudas Monto
distribuirEntrePonderados (Monto total) deudas =
  let
    deudaPairs = deudas & deudasToPairs
    cuotas = deudaPairs & fmap snd
    maxPrecision = cuotas
      & fmap Decimal.decimalPlaces
      & maximum
    participantes = deudaPairs & fmap fst
  in cuotas
      & fmap (Decimal.decimalMantissa . Decimal.roundTo maxPrecision)
      & Decimal.allocate total
      & fmap Monto
      & zip participantes
      & Map.fromList
      & Deudas

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

solveOptimalTransactions :: Deudas Monto -> [Transaccion]
solveOptimalTransactions (Deudas oldBalances) = unsafePerformIO $ do
    let
      maxPrecision =
        oldBalances
        & Map.elems
        & fmap Monto.getLugaresDespuesDeLaComa
        & \case
            [] -> 0
            xs -> maximum xs
      balances :: Map ParticipanteId Double
      balances = fmap (realToFrac . (* 10 ^ maxPrecision)) oldBalances

      balanceSum = sum $ Map.elems balances
      debtorMap = Map.filter (< 0) balances
      creditorMap = Map.filter (> 0) balances
      debtors = Map.keys debtorMap
      creditors = Map.keys creditorMap

    -- If no debts, no transactions needed
    if | balanceSum /= 0 -> error $ "Balanace is not 0, instead is: " <> show balanceSum
       | Map.null debtorMap -> pure []
       | otherwise -> do
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
                    , transaccionMonto = Monto $ Decimal.Decimal maxPrecision (round v)
                    })
                  )
              & pure
          _ -> do
            error $ "Failed resolving deudas: " <> show r

between :: (Ord a, Num a) => a -> a -> GLPK.Bounds a
between a delta =
  case compare delta 0 of
    EQ -> GLPK.Equ a
    LT -> GLPK.Bound (a + delta) a
    GT -> GLPK.Bound a (a + delta)


totalRepartija :: Repartija -> Monto
totalRepartija repartija =
  repartija.items
  & fmap (.monto)
  & sum
  & (+ repartija.extra)

calcularDeudasRepartija :: Repartija -> Deudas Monto
calcularDeudasRepartija repartija =
  let deudasIncluyendoNoRepartido =
        repartija.items
        & fmap (\item ->
              let claims =
                    repartija.claims
                      & filter ((== item.id) . (.itemId))
              in
                if | all tieneCantidad claims ->
                      let claimsExplicitos = claims
                            & fmap (\claim ->
                              mkDeuda claim.participante (fromMaybe (error "tieneCantidad") claim.cantidad))
                          claimsSobrante = item.cantidad - totalDeudas (mconcat claimsExplicitos)
                      in claimsExplicitos
                            & mconcat
                            & (<> if claimsSobrante /= 0 then mkDeuda (ParticipanteId nullUlid) claimsSobrante else mempty)
                            & fmap fromIntegral
                            & distribuirEntrePonderados item.monto

                   | (not (any tieneCantidad claims)) ->
                      claims
                        & fmap (\claim ->
                          mkDeuda claim.participante (maybe 1 fromIntegral claim.cantidad))
                        & mconcat
                        & distribuirEntrePonderados item.monto
                   | otherwise -> undefined
                )
        where
          tieneCantidad :: RepartijaClaim -> Bool
          tieneCantidad = isJust . (.cantidad)
      (montoNoRepartido, deudas) =
        deudasIncluyendoNoRepartido
        & fmap (extraerDeudor (ParticipanteId nullUlid))
        & unzip
      deudasDelExtraPonderado =
        deudas
        & mconcat
        & fmap inMonto
        & distribuirEntrePonderados (repartija.extra + sum montoNoRepartido)
        & filterDeudas (/= 0)

  in deudas
      & mconcat
      & (<> deudasDelExtraPonderado)

Elm.deriveBoth Elm.defaultOptions ''Transaccion
Elm.deriveBoth Elm.defaultOptions ''Deudas
Elm.deriveBoth Elm.defaultOptions ''MontoEspecifico
Elm.deriveBoth Elm.defaultOptions ''DistribucionMontosEspecificos
Elm.deriveBoth Elm.defaultOptions ''DistribucionMontoEquitativo
Elm.deriveBoth Elm.defaultOptions ''TipoDistribucion
Elm.deriveBoth Elm.defaultOptions ''Distribucion
Elm.deriveBoth Elm.defaultOptions ''ErrorResumen
Elm.deriveBoth Elm.defaultOptions ''ResumenDeudas
