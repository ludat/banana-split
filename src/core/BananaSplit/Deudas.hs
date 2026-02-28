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

import Elm.Derive qualified as Elm

import Protolude
import Protolude.Error (error)


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
                  then Transaccion Nothing partner personOwing paymentAmount
                  else Transaccion Nothing personOwing partner paymentAmount
          in fmap (transaction :) (settleDebts nextBalances)
    -- Helper to insert updated balances (or remove if settled)
    insertOrRemove :: (ParticipanteId, Monto) -> [(ParticipanteId, Monto)] -> [(ParticipanteId, Monto)]
    insertOrRemove (_, 0) balances = balances
    insertOrRemove updatedBalance balances = updatedBalance : deleteByPerson (fst updatedBalance) balances

    deleteByPerson :: ParticipanteId -> [(ParticipanteId, Monto)] -> [(ParticipanteId, Monto)]
    deleteByPerson name = filter ((/= name) . fst)

data Transaccion = Transaccion
  { transaccionId :: Maybe ULID
  , transaccionFrom :: ParticipanteId
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
          LT -> Transaccion Nothing mayorDeudor mayorPagador mayorDeuda
            : resolverNetosNaif (deudas'' <> mkDeuda mayorPagador (mayorPagado - mayorDeuda))
          GT -> Transaccion Nothing mayorDeudor mayorPagador mayorPagado
            : resolverNetosNaif (deudas'' <> mkDeuda mayorDeudor (-mayorDeuda + mayorPagado))
          EQ -> Transaccion Nothing mayorDeudor mayorPagador mayorPagado
            : resolverNetosNaif deudas''

solveOptimalTransactions' :: Netos Monto -> Either Text [Transaccion]
solveOptimalTransactions' (Netos oldBalances) =
  let
    maxPrecision =
      oldBalances
      & Map.elems
      & fmap Monto.getLugaresDespuesDeLaComa
      & \case
          [] -> 0
          xs -> maximum xs
    -- Scale all amounts to integers to avoid floating-point issues
    scaledBalances :: Map ParticipanteId Integer
    scaledBalances = Map.map
      (\m -> Decimal.decimalMantissa $ Decimal.roundTo maxPrecision $ inMonto m)
      oldBalances
    balanceSum = sum $ Map.elems scaledBalances
    debtors    = Netos $ Map.filter (< 0) scaledBalances
    creditors  = Netos $ Map.filter (> 0) scaledBalances
    toMonto scaledAmount = Monto $ Decimal.Decimal maxPrecision scaledAmount
  in
    if balanceSum /= 0
    then error $ "Balance is not 0, instead is: " <> show (inMonto (sum $ Map.elems oldBalances))
    else Right $
      decomposeSubgroups debtors creditors
      & concatMap (uncurry solveSubgroupOptimal)
      & fmap (\(debtorId, creditorId, scaledAmount) -> Transaccion Nothing debtorId creditorId (toMonto scaledAmount))

-- | Decompose into independent subgroups via subset-sum matching.
-- Each subgroup's debts can be settled independently without involving
-- participants from other subgroups.
decomposeSubgroups
  :: Netos Integer
  -> Netos Integer
  -> [(Netos Integer, Netos Integer)]
decomposeSubgroups initialDebtors initialCreditors = go initialDebtors initialCreditors
  where
    go debtors@(Netos debtorMap) creditors@(Netos creditorMap)
      | Map.null debtorMap && Map.null creditorMap = []
      | Map.null creditorMap = error $ "decomposeSubgroups: debtors without creditors (bug in data): " <> show (Map.keys debtorMap)
      | Map.null debtorMap   = error $ "decomposeSubgroups: creditors without debtors (bug in data): " <> show (Map.keys creditorMap)
      | otherwise =
          case findIndependentSubgroup (Map.toList debtorMap) (Map.toList creditorMap) of
            Nothing ->
              [(debtors, creditors)]
            Just (debtorSubset, creditorSubset) ->
              (Netos (Map.fromList debtorSubset), Netos (Map.fromList creditorSubset))
              : go (Netos (Map.difference debtorMap (Map.fromList debtorSubset)))
                   (Netos (Map.difference creditorMap (Map.fromList creditorSubset)))

    findIndependentSubgroup
      :: [(ParticipanteId, Integer)]
      -> [(ParticipanteId, Integer)]
      -> Maybe ([(ParticipanteId, Integer)], [(ParticipanteId, Integer)])
    findIndependentSubgroup allDebtorPairs allCreditorPairs =
      let creditorSubsets      = List.subsequences allCreditorPairs
                                   & filter (\subset -> not (null subset) && length subset < length allCreditorPairs)
          creditorSubsetsBySum = Map.fromListWith (++)
                                   [(sum (fmap snd subset), [subset]) | subset <- creditorSubsets]
          debtorCount          = length allDebtorPairs
      in listToMaybe
           [ (debtorSubset, creditorSubset)
           | subsetSize              <- [1 .. debtorCount - 1]
           , debtorSubset            <- List.subsequences allDebtorPairs & filter ((== subsetSize) . length)
           , let debtorSubsetAbsSum  = negate (sum (fmap snd debtorSubset))
           , debtorSubsetAbsSum > 0
           , matchingCreditorSubsets <- maybeToList (Map.lookup debtorSubsetAbsSum creditorSubsetsBySum)
           , creditorSubset          <- matchingCreditorSubsets
           ]

-- | Solve a single balanced subgroup using recursive search with upper-bound pruning.
-- Debtors have negative balances, creditors have positive balances.
-- Returns (debtorId, creditorId, paymentAmount) triples.
solveSubgroupOptimal
  :: Netos Integer
  -> Netos Integer
  -> [(ParticipanteId, ParticipanteId, Integer)]
solveSubgroupOptimal (Netos debtorMap) (Netos creditorMap)
  | Map.null debtorMap   = []
  | Map.null creditorMap = []
  | otherwise =
      let debtorPairs     = Map.toList debtorMap
          creditorPairs   = Map.toList creditorMap
          initialSolution = twoPointerSolve debtorPairs creditorPairs
      in search debtorPairs creditorPairs [] initialSolution
  where
    search
      :: [(ParticipanteId, Integer)] -> [(ParticipanteId, Integer)]
      -> [(ParticipanteId, ParticipanteId, Integer)]
      -> [(ParticipanteId, ParticipanteId, Integer)]
      -> [(ParticipanteId, ParticipanteId, Integer)]
    search [] _ currentTransactions bestSolution
      | length currentTransactions < length bestSolution = currentTransactions
      | otherwise                                        = bestSolution
    search _ [] currentTransactions bestSolution
      | length currentTransactions < length bestSolution = currentTransactions
      | otherwise                                        = bestSolution
    search ((debtorId, debtorBalance):remainingDebtors) creditors currentTransactions bestSolution =
      foldl' trySettlingWithCreditor bestSolution creditors
      where
        trySettlingWithCreditor currentBest (creditorId, creditorBalance) =
          let paymentAmount          = min (abs debtorBalance) creditorBalance
              debtorBalanceAfter     = debtorBalance + paymentAmount
              creditorBalanceAfter   = creditorBalance - paymentAmount
              remainingDebtorCount   = length remainingDebtors + if debtorBalanceAfter /= 0 then 1 else 0
              remainingCreditorCount = length creditors - 1 + if creditorBalanceAfter /= 0 then 1 else 0
              transactionLowerBound  = max remainingDebtorCount remainingCreditorCount
              updatedDebtors         = if debtorBalanceAfter == 0
                                       then remainingDebtors
                                       else (debtorId, debtorBalanceAfter) : remainingDebtors
              updatedCreditors       = [(pid, bal) | (pid, bal) <- creditors, pid /= creditorId]
                                       <> [(creditorId, creditorBalanceAfter) | creditorBalanceAfter /= 0]
              newTransaction         = (debtorId, creditorId, paymentAmount)
          in if length currentTransactions + 1 + transactionLowerBound >= length currentBest
             then currentBest
             else search updatedDebtors updatedCreditors (newTransaction : currentTransactions) currentBest

-- | Sorted two-pointer: produces at most |D|+|C|-1 transactions in O(n log n).
-- Used as the initial upper bound for the recursive search in 'solveSubgroupOptimal'.
twoPointerSolve
  :: [(ParticipanteId, Integer)]
  -> [(ParticipanteId, Integer)]
  -> [(ParticipanteId, ParticipanteId, Integer)]
twoPointerSolve debtorPairs creditorPairs = go sortedDebtors sortedCreditors
  where
    sortedDebtors  = List.sortBy (comparing (Down . abs . snd)) debtorPairs
    sortedCreditors = List.sortBy (comparing (Down . snd)) creditorPairs
    go [] _ = []
    go _ [] = []
    go ((debtorId, debtorBalance):remainingDebtors) ((creditorId, creditorBalance):remainingCreditors) =
      let paymentAmount       = min (abs debtorBalance) creditorBalance
          debtorBalanceAfter  = debtorBalance + paymentAmount
          creditorBalanceAfter = creditorBalance - paymentAmount
      in (debtorId, creditorId, paymentAmount) :
           if | debtorBalanceAfter == 0 && creditorBalanceAfter == 0 -> go remainingDebtors remainingCreditors
              | debtorBalanceAfter == 0                              -> go remainingDebtors ((creditorId, creditorBalanceAfter) : remainingCreditors)
              | otherwise                                            -> go ((debtorId, debtorBalanceAfter) : remainingDebtors) remainingCreditors

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
