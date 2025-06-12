module BananaSplit.TestUtils
    ( fakeUlid
    , participante
    ) where

import BananaSplit.Core

import Data.ULID

import Protolude
import Protolude.Error

participante :: Integer -> ParticipanteId
participante = ParticipanteId . fakeUlid

fakeUlid :: Integer -> ULID
fakeUlid integer =
  case ulidFromInteger integer of
    Right ulid -> ulid
    Left e -> error e
