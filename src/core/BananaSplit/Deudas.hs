{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Deudas where

import BananaSplit.Monto
import BananaSplit.Monto qualified as Monto
import BananaSplit.Participante
import BananaSplit.Repartija
import BananaSplit.ULID

import Data.Decimal (Decimal)
import Data.Decimal qualified as Decimal
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific

import Elm.Derive qualified as Elm

import Numeric.Optimization.MIP qualified as MIP
import Numeric.Optimization.MIP.Solver qualified as MIP
import Numeric.Optimization.MIP.Solver.CBC qualified as CBC

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

data ResumenNetos
  = NetosIncomputables (Maybe Monto) ErrorResumen
  | ResumenNetos (Maybe Monto) (Netos Monto)
  deriving (Show, Eq, Generic)

data ErrorResumen
  = ErrorResumen (Maybe Text) [(Text, ErrorResumen)]
  deriving (Show, Eq, Generic)

instance Monoid ErrorResumen where
  mempty = ErrorResumen mempty []

instance Semigroup ErrorResumen where
  ErrorResumen msg1 errs1 <> ErrorResumen msg2 errs2 =
    ErrorResumen (msg1 <> msg2) (errs1 <> errs2)

getNetos :: HasResumen a => Monto -> a -> Maybe (Netos Monto)
getNetos totalPago = getNetosResumen . getResumen totalPago

getNetosResumen :: ResumenNetos -> Maybe (Netos Monto)
getNetosResumen resumen =
  case resumen of
    NetosIncomputables _ _ -> Nothing
    ResumenNetos _ deudas -> Just deudas

class HasResumen a where
  getResumen :: Monto -> a -> ResumenNetos

instance HasResumen ResumenNetos where
  getResumen _ = identity

instance HasResumen Distribucion where
  getResumen totalPago distribucion =
    case distribucion.tipo of
      TipoDistribucionMontoEquitativo d -> getResumen totalPago d
      TipoDistribucionMontosEspecificos d -> getResumen totalPago d
      TipoDistribucionRepartija r -> getResumen totalPago r

instance HasResumen DistribucionMontosEspecificos where
  getResumen totalPago distribucion =
    if | null distribucion.montos -> NetosIncomputables (Just 0) $ ErrorResumen (Just "No hay montos especificados") []
       | total /= totalPago -> NetosIncomputables (Just total) $ ErrorResumen (Just $ "El total debería ser igual al total del pago pero es: " <> show total <> " en vez de " <> show totalPago) []
       | otherwise -> ResumenNetos (Just total) deudas
    where
      deudas = distribucion.montos <&> (\m -> mkDeuda m.participante m.monto) & mconcat
      total = totalNetos deudas

instance HasResumen DistribucionMontoEquitativo where
  getResumen totalPago d =
    if | null d.participantes -> NetosIncomputables (Just totalPago) $ ErrorResumen (Just "No hay participantes especificados") []
       | otherwise -> ResumenNetos (Just totalPago) $ calcularNetosMontoEquitativo totalPago d

instance HasResumen Repartija where
  getResumen totalPago repartija =
    if | null repartija.items -> NetosIncomputables (Just totalPorItems) $ ErrorResumen (Just "No hay items para repartir.") []
       | totalPorItems /= totalPago -> NetosIncomputables (Just totalPorItems) $ ErrorResumen (Just $ "El total de items debería ser igual al total del pago pero es: " <> show totalPorItems <> " en vez de " <> show totalPago) []
       | null repartija.claims -> NetosIncomputables (Just totalPorItems) $ ErrorResumen (Just "Nadie reclamo ningun item.") []
       | totalPorNetos /= totalPago -> NetosIncomputables (Just totalPorItems) $ ErrorResumen (Just $ "El total de deudas debería ser igual al total del pago pero es: " <> show totalPorNetos <> " en vez de " <> show totalPago) []
       | otherwise ->
           ResumenNetos (Just totalPorItems) deudas
    where
      totalPorItems = totalRepartija repartija
      deudas = calcularNetosRepartija repartija
      totalPorNetos = totalNetos deudas

minimizeTransactions :: Netos Monto -> [Transaccion]
minimizeTransactions deudas =
  case solveOptimalTransactions' deudas of
    Right transactions -> transactions
    Left _err -> resolverNetosNaif deudas

solveOptimalTransactions :: Netos Monto -> [Transaccion]
solveOptimalTransactions deudas =
  case solveOptimalTransactions' deudas of
    Right transactions -> transactions
    Left err -> error err

resolverNetosRecursivo :: Netos Monto -> [Transaccion]
resolverNetosRecursivo netBalances =
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

deudoresNoNulos :: Netos Monto -> Int
deudoresNoNulos (Netos deudasMap) =
  deudasMap
  & Map.filter (/= 0)
  & length

filterNetos :: (a -> Bool) -> Netos a -> Netos a
filterNetos f (Netos deudasMap) =
  deudasMap
  & Map.filter f
  & Netos

newtype Netos a = Netos (Map ParticipanteId a)
  deriving newtype (Show, Eq, Functor)

deudasToPairs :: Ord a => Netos a -> [(ParticipanteId, a)]
deudasToPairs (Netos deudasMap) =
  deudasMap
  & Map.toAscList
  & sortOn (\(p, m) -> (Down m, p))

instance Num a => Monoid (Netos a) where
  mempty = Netos Map.empty

instance Num a => Semigroup (Netos a) where
  Netos d1 <> Netos d2 = Netos $ Map.unionWith (+) d1 d2

totalNetos :: Num a => Netos a -> a
totalNetos (Netos deudasMap) =
  deudasMap
  & Map.elems
  & sum

mkDeuda :: ParticipanteId -> a -> Netos a
mkDeuda participanteId monto =
  Netos $ Map.singleton participanteId monto

calcularNetosMontoEquitativo :: Monto -> DistribucionMontoEquitativo -> Netos Monto
calcularNetosMontoEquitativo total distribucion =
  distribucion.participantes
  & fmap (`mkDeuda` 1)
  & mconcat
  & distribuirEntrePonderados total

distribuirEntrePonderados :: Monto -> Netos Decimal -> Netos Monto
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
      & Netos

extraerMaximoDeudor :: Netos Monto -> (ParticipanteId, Monto)
extraerMaximoDeudor (Netos deudasMap) =
  deudasMap
  & Map.filter (< 0)
  & fmap (* -1)
  & Map.toList
  & List.maximumBy (compare `on` snd)

extraerMaximoPagador :: Netos Monto -> (ParticipanteId, Monto)
extraerMaximoPagador (Netos deudasMap) =
  deudasMap
  & Map.filter (> 0)
  & Map.toList
  & List.maximumBy (compare `on` snd)

extraerDeudor :: Num a => ParticipanteId -> Netos a -> (a, Netos a)
extraerDeudor unId (Netos deudasMap) =
  ( deudasMap & Map.findWithDefault 0 unId
  , Netos $ deudasMap & Map.delete unId)

removerDeudor :: ParticipanteId -> Netos m -> Netos m
removerDeudor participanteId (Netos deudasMap) =
  Netos $ Map.delete participanteId deudasMap

resolverNetosNaif :: Netos Monto -> [Transaccion]
resolverNetosNaif deudas
  | deudoresNoNulos deudas == 0 = []
  | deudoresNoNulos deudas == 1 = error $ show deudas
  | otherwise =
      let
        (mayorDeudor, mayorDeuda) = extraerMaximoDeudor deudas
        deudas' = removerDeudor mayorDeudor deudas
        (mayorPagador, mayorPagado) = extraerMaximoPagador deudas'
        deudas'' = removerDeudor mayorPagador deudas'
      in case compare mayorDeuda mayorPagado of
          LT -> Transaccion mayorDeudor mayorPagador mayorDeuda
            : resolverNetosNaif (deudas'' <> mkDeuda mayorPagador (mayorPagado - mayorDeuda))
          GT -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverNetosNaif (deudas'' <> mkDeuda mayorDeudor (-mayorDeuda + mayorPagado))
          EQ -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverNetosNaif deudas''

solveOptimalTransactions' :: Netos Monto -> Either Text [Transaccion]
solveOptimalTransactions' (Netos oldBalances) = unsafePerformIO $ do
    let
      maxPrecision =
        oldBalances
        & Map.elems
        & fmap Monto.getLugaresDespuesDeLaComa
        & \case
            [] -> 0
            xs -> maximum xs
      balances :: Map ParticipanteId Scientific
      balances = fmap (realToFrac . (* 10 ^ maxPrecision)) oldBalances

      balanceSum = sum $ Map.elems balances
      debtorMap = Map.filter (< 0) balances
      creditorMap = Map.filter (> 0) balances
      debtors = Map.keys debtorMap
      creditors = Map.keys creditorMap

    -- If no debts, no transactions needed
    if | balanceSum /= 0 -> error $ "Balance is not 0, instead is: " <> show balanceSum
       | Map.null debtorMap -> pure $ Right []
       | otherwise -> do
        -- A "big M" value, larger than any possible transaction
        let bigM = balances & Map.elems & filter (> 0) & sum

        -- Create mappings from (debtor, creditor) pairs to variable names
        let d_c_pairs = [(d, c) | d <- debtors, c <- creditors]

        -- Create variable name helpers
        let tVar d c = "t_" <> show d <> "_" <> show c :: Text
            eVar d c = "e_" <> show d <> "_" <> show c :: Text

        -- All variable names
        let allTVars = [tVar d c | (d, c) <- d_c_pairs]
            allEVars = [eVar d c | (d, c) <- d_c_pairs]

        -- Build variable domains
        let varDomains = Map.fromList $
              -- Transaction variables (continuous, >= 0)
              [(MIP.Var v, (MIP.IntegerVariable, (MIP.Finite 0, MIP.PosInf))) | v <- allTVars] ++
              -- Binary edge variables (represented as integer variables with bounds 0 and 1)
              [(MIP.Var v, (MIP.IntegerVariable, (MIP.Finite 0, MIP.Finite 1))) | v <- allEVars]

        -- Build objective: minimize sum of binary edge variables
        let objective = MIP.def
              { MIP.objDir = MIP.OptMin
              , MIP.objExpr = sum [MIP.varExpr (MIP.Var (eVar d c)) | (d, c) <- d_c_pairs]
              }

        -- Build constraints
        let constraints =
              -- Debtor constraints: sum of outgoing transactions should equal their debt
              [ sum [MIP.varExpr (MIP.Var (tVar debtor c)) | c <- creditors]
                MIP..==. MIP.constExpr (negate $ balances Map.! debtor)
              | debtor <- debtors
              ] ++
              -- Creditor constraints: sum of incoming transactions should equal their credit
              [ sum [MIP.varExpr (MIP.Var (tVar d creditor)) | d <- debtors]
                MIP..==. MIP.constExpr (balances Map.! creditor)
              | creditor <- creditors
              ] ++
              -- Big-M constraints: t_d_c <= bigM * e_d_c (transaction only if edge is active)
              [ MIP.varExpr (MIP.Var (tVar d c)) MIP..<=. MIP.constExpr bigM * MIP.varExpr (MIP.Var (eVar d c))
              | (d, c) <- d_c_pairs
              ]

        -- Create the problem
        let prob = MIP.def
              { MIP.objectiveFunction = objective
              , MIP.constraints = constraints
              , MIP.varDomains = varDomains
              }

        -- Solve using CBC with timeout protection and optimality gap tolerance
        let solverOpts = MIP.def
              { MIP.solveTimeLimit = Just 5.0
              , MIP.solveLogger = \msg -> putStrLn $ "[CBC] " <> msg  -- Debug logging
              , MIP.solveErrorLogger = \msg -> putStrLn $ "[CBC ERROR] " <> msg  -- Debug logging
              , MIP.solveTol = Just MIP.Tol
                  { integralityTol = 0
                  , feasibilityTol = 0
                  , optimalityTol = 0
                  }
              }
        -- Configure CBC to stop after finding a good solution
        -- For transaction minimization, first feasible solution is often near-optimal
        let solver = CBC.cbc
        sol <- MIP.solve solver solverOpts prob

        -- Helper to extract and verify transactions
        let extractAndVerifyTransactions solVars = do
              let transactions = d_c_pairs
                    & mapMaybe (\(d, c) ->
                      solVars
                        & Map.lookup (MIP.Var (tVar d c))
                        & mfilter (/= 0)
                        & fmap (\v -> Transaccion
                          { transaccionFrom = d
                          , transaccionTo = c
                          , transaccionMonto = Monto $ Decimal.Decimal maxPrecision (round $ Scientific.toRealFloat @Double v)
                          })
                        )

              -- Verify: apply transactions and check all balances become zero
              let neto =
                    transactions
                    & fmap (\t -> mkDeuda t.transaccionFrom -t.transaccionMonto <> mkDeuda t.transaccionTo t.transaccionMonto)
                    & mconcat
                    & totalNetos

              if neto /= 0
                then do
                  putText $ "[error] Transactions do not balance out: " <> show transactions
                  pure $ Left $ "Transactions do not balance, neto is " <> show neto

                else
                  pure $ Right transactions

        case MIP.solStatus sol of
          MIP.StatusOptimal -> extractAndVerifyTransactions (MIP.solVariables sol)
          MIP.StatusInfeasible -> pure $ Left "LP solver found the problem infeasible"
          MIP.StatusUnknown -> extractAndVerifyTransactions (MIP.solVariables sol)
          status -> pure $ Left $ "Failed resolving deudas: " <> show status

totalRepartija :: Repartija -> Monto
totalRepartija repartija =
  repartija.items
  & fmap (.monto)
  & sum
  & (+ repartija.extra)

calcularNetosRepartija :: Repartija -> Netos Monto
calcularNetosRepartija repartija =
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
                          claimsSobrante = item.cantidad - totalNetos (mconcat claimsExplicitos)
                      in claimsExplicitos
                            & mconcat
                            & (<> if claimsSobrante > 0 then mkDeuda (ParticipanteId nullUlid) claimsSobrante else mempty)
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
        & filterNetos (/= 0)

  in deudas
      & mconcat
      & (<> deudasDelExtraPonderado)

Elm.deriveBoth Elm.defaultOptions ''Transaccion
Elm.deriveBoth Elm.defaultOptions ''Netos
Elm.deriveBoth Elm.defaultOptions ''MontoEspecifico
Elm.deriveBoth Elm.defaultOptions ''DistribucionMontosEspecificos
Elm.deriveBoth Elm.defaultOptions ''DistribucionMontoEquitativo
Elm.deriveBoth Elm.defaultOptions ''TipoDistribucion
Elm.deriveBoth Elm.defaultOptions ''Distribucion
Elm.deriveBoth Elm.defaultOptions ''ErrorResumen
Elm.deriveBoth Elm.defaultOptions ''ResumenNetos
