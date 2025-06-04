{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Repartija where

import BananaSplit.Core
import BananaSplit.Solver (Deudas, deudasToPairs, distribuirEntrePonderados, extraerDeudor,
                           filterDeudas, mkDeuda, totalDeudas)

import Data.ULID (ULID)

import Elm.Derive qualified as Elm

import Protolude
import Protolude.Error


data Repartija = Repartija
  { repartijaId :: ULID
  , repartijaGrupoId :: ULID
  , repartijaNombre :: Text
  , repartijaExtra :: Monto
  , repartijaItems :: [RepartijaItem]
  , repartijaClaims :: [RepartijaClaim]
  } deriving (Show, Eq, Generic)

data ShallowRepartija = ShallowRepartija
  { repartijaShallowId :: ULID
  , repartijaShallowNombre :: Text
  } deriving (Show, Eq, Generic)

data RepartijaItem = RepartijaItem
  { repartijaItemId :: ULID
  , repartijaItemNombre :: Text
  , repartijaItemMonto :: Monto
  , repartijaItemCantidad :: Int
  } deriving (Show, Eq, Generic)

data RepartijaClaim = RepartijaClaim
  { repartijaClaimId :: ULID
  , repartijaClaimParticipante :: ParticipanteId
  , repartijaClaimItemId :: ULID
  , repartijaClaimCantidad :: Maybe Int
  } deriving (Show, Eq, Generic)


repartija2Pago :: Repartija -> Pago
repartija2Pago repartija =
  let totalItems = repartija.repartijaItems
        & fmap (.repartijaItemMonto)
        & sum
      total = repartija.repartijaExtra + totalItems
      deudasIncluyendoNoRepartido =
        repartija.repartijaItems
        & fmap (\item ->
              let claims =
                    repartija.repartijaClaims
                      & filter ((== item.repartijaItemId) . (.repartijaClaimItemId))
              in
                if | all tieneCantidad claims ->
                      let claimsExplicitos = claims
                            & fmap (\claim ->
                              mkDeuda claim.repartijaClaimParticipante (fromMaybe (error "tieneCantidad") claim.repartijaClaimCantidad))
                          claimsSobrante = item.repartijaItemCantidad - totalDeudas (mconcat claimsExplicitos)
                      in claimsExplicitos
                            & mconcat
                            & (<> if claimsSobrante /= 0 then mkDeuda (ParticipanteId nullUlid) claimsSobrante else mempty)
                            & distribuirEntrePonderados item.repartijaItemMonto

                   | (not (any tieneCantidad claims)) ->
                      claims
                        & fmap (\claim ->
                          mkDeuda claim.repartijaClaimParticipante (fromMaybe 1 claim.repartijaClaimCantidad))
                        & mconcat
                        & distribuirEntrePonderados item.repartijaItemMonto
                   | otherwise -> undefined
                )
        where
          tieneCantidad :: RepartijaClaim -> Bool
          tieneCantidad = isJust . (.repartijaClaimCantidad)
      (montoNoRepartido, deudas) =
        deudasIncluyendoNoRepartido
        & fmap (extraerDeudor (ParticipanteId nullUlid))
        & unzip
      deudasDelExtraPonderado =
        deudas
        & mconcat
        & distribuirEntrePonderados (repartija.repartijaExtra + sum montoNoRepartido)
        & filterDeudas (/= 0)
        & deudasToPartes

  in Pago
  { pagoId = nullUlid
  , monto = total
  , nombre = repartija.repartijaNombre
  , deudores =
    deudas
    & fmap deudasToPartes
    & mconcat
    & (<> deudasDelExtraPonderado)
  , pagadores = []
  }
  where
    deudasToPartes :: Deudas Monto -> [Parte]
    deudasToPartes =
      fmap (\(participanteId, monto) -> MontoFijo monto participanteId) . deudasToPairs

Elm.deriveBoth Elm.defaultOptions ''RepartijaItem
Elm.deriveBoth Elm.defaultOptions ''RepartijaClaim
Elm.deriveBoth Elm.defaultOptions ''ShallowRepartija
Elm.deriveBoth Elm.defaultOptions ''Repartija
