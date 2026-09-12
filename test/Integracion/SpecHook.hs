module Integracion.SpecHook (
  hook,
) where

import Test.Hspec

import BaseDePrueba (prepararBase)

hook :: Spec -> Spec
hook = beforeAll_ prepararBase
