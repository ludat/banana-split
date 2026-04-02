module BananaSplit.DeudasSpec (
  spec,
) where

import Data.Decimal qualified as Decimal
import Protolude
import Test.Hspec
import Test.QuickCheck

import BananaSplit
import BananaSplit.TestUtils

spec :: Spec
spec = do
  let
    u1 = participante 1
    u2 = participante 2
    u3 = participante 3
    u4 = participante 4
    u5 = participante 5
    u6 = participante 6
    pagoValido :: Pago
    pagoValido =
      Pago
        { pagoId = fakeUlid 100
        , monto = 1000
        , isValid = True
        , nombre = "Pago"
        , pagadores = distribucionMontosEspecificos []
        , deudores = distribucionMontosEspecificos []
        }

  let
    fakeRepartija =
      Repartija
        { id = fakeUlid 50
        , nombre = "test"
        , extra = 0
        , distribucionDeSobras = SobrasNoDistribuir
        , claims = []
        , items = []
        }
    err objeto tipo = ErrorResumen{objeto = objeto, tipo = tipo}

  describe "getResumen para DistribucionMontosEspecificos" $ do
    it "sin montos devuelve ErrorMontosEspecificosVacios" $ do
      (getResumen 100 (distribucionMontosEspecificos [])).errores
        `shouldBe` [err [] ErrorMontosEspecificosVacios]

    it "con total que no coincide devuelve ErrorMontosEspecificosTotalNoCoincide" $ do
      (getResumen 500 (distribucionMontosEspecificos [(u1, 300)])).errores
        `shouldBe` [err [] (ErrorMontosEspecificosTotalNoCoincide 300 500)]

    it "con total que coincide no devuelve errores" $ do
      (getResumen 500 (distribucionMontosEspecificos [(u1, 300), (u2, 200)])).errores
        `shouldBe` []

  describe "getResumen para DistribucionMontoEquitativo" $ do
    it "sin participantes devuelve ErrorEquitativoSinParticipantes" $ do
      (getResumen 100 (distribucionMontoEquitativo [])).errores
        `shouldBe` [err [] ErrorEquitativoSinParticipantes]

    it "con participantes no devuelve errores" $ do
      (getResumen 100 (distribucionMontoEquitativo [u1, u2])).errores
        `shouldBe` []

  describe "getResumen para Repartija" $ do
    it "sin items devuelve ErrorRepartijaSinItems" $ do
      (getResumen 100 (distribucionRepartija fakeRepartija)).errores
        `shouldBe` [err [] ErrorRepartijaSinItems]

    it "con total de items que no coincide devuelve ErrorRepartijaTotalItemsNoCoincide" $ do
      let r =
            fakeRepartija
              { items = [RepartijaItem (fakeUlid 2) "Item" 300 1]
              , claims = [RepartijaClaim (fakeUlid 100) u1 (fakeUlid 2) (Just 1)]
              }
      (getResumen 500 (distribucionRepartija r)).errores
        `shouldBe` [err [] (ErrorRepartijaTotalItemsNoCoincide 300 500)]

    it "sin claims devuelve ErrorRepartijaSinClaims" $ do
      let r =
            fakeRepartija
              { items = [RepartijaItem (fakeUlid 2) "Item" 500 1]
              }
      (getResumen 500 (distribucionRepartija r)).errores
        `shouldBe` [err [] ErrorRepartijaSinClaims]

    it "con total reclamado que no coincide devuelve ErrorRepartijaTotalReclamadoNoCoincide" $ do
      let item1 = fakeUlid 2
          item2 = fakeUlid 3
          r =
            fakeRepartija
              { items =
                  [ RepartijaItem item1 "Item1" 300 2
                  , RepartijaItem item2 "Item2" 200 1
                  ]
              , claims =
                  [ RepartijaClaim (fakeUlid 100) u1 item1 (Just 1)
                  ]
              }
      let res = getResumen 500 (distribucionRepartija r)
      res.errores
        `shouldBe` [err [] (ErrorRepartijaTotalReclamadoNoCoincide (totalNetos res.netos) 500)]

    it "con todo bien no devuelve errores" $ do
      let item1 = fakeUlid 2
          r =
            fakeRepartija
              { items = [RepartijaItem item1 "Item" 500 1]
              , claims = [RepartijaClaim (fakeUlid 100) u1 item1 (Just 1)]
              }
      (getResumen 500 (distribucionRepartija r)).errores
        `shouldBe` []

    it "SobrasProporcional reparte items no reclamados proporcionalmente" $ do
      let item1 = fakeUlid 2
          item2 = fakeUlid 3
          r =
            fakeRepartija
              { distribucionDeSobras = SobrasProporcional
              , items =
                  [ RepartijaItem item1 "Birrita" 200 2
                  , RepartijaItem item2 "Papitas" 300 1
                  ]
              , claims =
                  [ RepartijaClaim (fakeUlid 100) u1 item1 (Just 1)
                  , RepartijaClaim (fakeUlid 101) u2 item1 (Just 1)
                  ]
              }
      (getResumen 500 (distribucionRepartija r)).errores
        `shouldBe` []

    it "SobrasNoDistribuir no reparte items no reclamados" $ do
      let item1 = fakeUlid 2
          item2 = fakeUlid 3
          r =
            fakeRepartija
              { distribucionDeSobras = SobrasNoDistribuir
              , items =
                  [ RepartijaItem item1 "Birrita" 200 2
                  , RepartijaItem item2 "Papitas" 300 1
                  ]
              , claims =
                  [ RepartijaClaim (fakeUlid 100) u1 item1 (Just 1)
                  , RepartijaClaim (fakeUlid 101) u2 item1 (Just 1)
                  ]
              }
          res = getResumen 500 (distribucionRepartija r)
      res.errores
        `shouldBe` [err [] (ErrorRepartijaTotalReclamadoNoCoincide (totalNetos res.netos) 500)]

    it "SobrasProporcional reparte propina proporcionalmente entre los que reclamaron" $ do
      let item1 = fakeUlid 2
          r =
            fakeRepartija
              { distribucionDeSobras = SobrasProporcional
              , extra = 100
              , items = [RepartijaItem item1 "Birrita" 400 2]
              , claims =
                  [ RepartijaClaim (fakeUlid 100) u1 item1 (Just 1)
                  , RepartijaClaim (fakeUlid 101) u2 item1 (Just 1)
                  ]
              }
          res = getResumen 500 (distribucionRepartija r)
      res.errores `shouldBe` []
      totalNetos res.netos `shouldBe` 500

    it "SobrasNoDistribuir reparte solo la propina, no los items sobrantes" $ do
      let item1 = fakeUlid 2
          r =
            fakeRepartija
              { distribucionDeSobras = SobrasNoDistribuir
              , extra = 50
              , items = [RepartijaItem item1 "Birrita" 200 2]
              , claims =
                  [ RepartijaClaim (fakeUlid 100) u1 item1 (Just 1)
                  ]
              }
          res = getResumen 250 (distribucionRepartija r)
      res.errores
        `shouldBe` [err [] (ErrorRepartijaTotalReclamadoNoCoincide (totalNetos res.netos) 250)]

  describe "getResumenPago" $ do
    it "con pagadores y deudores que no balancean devuelve errores con objeto correcto" $ do
      let pago =
            pagoValido
              { monto = 500
              , pagadores = distribucionMontosEspecificos [(u1, 500)]
              , deudores = distribucionMontosEspecificos [(u2, 300)]
              }
          res = getResumenPago pago
      res.errores
        `shouldBe` [ err ["deudores"] (ErrorMontosEspecificosTotalNoCoincide 300 500)
                   ]

    it "con pagadores y deudores que balancean no devuelve errores" $ do
      let pago =
            pagoValido
              { monto = 500
              , pagadores = distribucionMontosEspecificos [(u1, 500)]
              , deudores = distribucionMontosEspecificos [(u2, 500)]
              }
      (getResumenPago pago).errores
        `shouldBe` []

    it "errores de pagadores tienen objeto 'pagadores'" $ do
      let pago =
            pagoValido
              { monto = 500
              , pagadores = distribucionMontosEspecificos []
              , deudores = distribucionMontosEspecificos [(u2, 500)]
              }
      (getResumenPago pago).errores
        `shouldBe` [err ["pagadores"] ErrorMontosEspecificosVacios]

  describe "calcularNetos" $ do
    it "calcula netos de un pago montos especificos en pagadores y deudores" $ do
      calcularNetosPago
        pagoValido
          { monto = 500
          , pagadores =
              distribucionMontosEspecificos
                [ (u1, 500)
                ]
          , deudores =
              distribucionMontosEspecificos
                [ (u2, 500)
                ]
          }
        `shouldBe` netos
          [ (u1, 500)
          , (u2, -500)
          ]

  describe "calcularNetosMontoEquitativo" $ do
    it "entre varias personas devuelve equitativo" $ do
      getNetos
        300
        (distribucionMontoEquitativo [u1, u2, u3])
        `shouldBe` Just
          ( netos
              [ (u1, 100)
              , (u2, 100)
              , (u3, 100)
              ]
          )
    it "calcular netos de un monto equitativo siempre suma el total de nuevo" $ property $ \(Positive m :: Positive Int) (NonEmpty us :: NonEmptyList (Positive Integer)) -> do
      let monto = fromIntegral @_ @Monto m
      let participants = us <&> participante . getPositive
      ( getNetos
          monto
          (distribucionMontoEquitativo participants)
          & fmap totalNetos
        )
        `shouldBe` Just monto

    it "con monto cero devuelve netos vacias" $ do
      pendingWith "Not sure if this is the right default right now"
      getNetos
        0
        (distribucionMontoEquitativo [u1, u2, u3])
        `shouldBe` Just (netos [])

  describe "simplify transactions" $ do
    it "simplifica ningun transaccion trivialmente" $ do
      minimizeTransactions mempty `shouldBe` []

    it "simplifica una sola transaccion trivialmente" $ do
      let transacciones = netos [(participante 1, 10), (participante 2, -10)]

      minimizeTransactions transacciones `shouldBe` [Transaccion Nothing (participante 2) (participante 1) 10]
    it "simplifica un caso en el que el algoritmo greedy falla" $ do
      minimizeTransactions
        ( netos
            [ (u1, 10)
            , (u2, -5)
            , (u3, -5)
            , (u4, 6)
            , (u5, -3)
            , (u6, -3)
            ]
        )
        `shouldSatisfy` ((== 4) . length)
    it "devuelve vacio si se le pasa vacio" $ do
      minimizeTransactions (netos []) `shouldBe` []
    it "simplifica un caso con numeros con coma" $ do
      minimizeTransactions
        ( netos
            [ (u1, mkMonto 2 66)
            , (u2, mkMonto 2 -33)
            , (u3, mkMonto 2 -33)
            ]
        )
        `shouldSatisfy` ((== 2) . length)
    context "con netos no coherentes (no suman 0 en total)" $ do
      it "con una deuda simple que esta desbalanceada" $ do
        evaluate
          ( minimizeTransactions
              ( netos
                  [ (u1, Monto $ Decimal.Decimal 0 10)
                  , (u2, Monto $ Decimal.Decimal 0 -11)
                  ]
              )
          )
          `shouldThrow` errorCall "Balance is not 0, instead is: -1.0"

      it "con una deuda compleja no crashea" $ do
        evaluate
          ( minimizeTransactions
              ( netos
                  [ (u1, 10)
                  , (u2, -5)
                  , (u3, -5)
                  , (u4, 6)
                  , (u5, -3)
                  , (u6, -2)
                  ]
              )
          )
          `shouldThrow` errorCall "Balance is not 0, instead is: 1.0"

      it "cuando una sola persona tiene plata a favor" $ do
        pendingWith "this crashes the solver"
        evaluate
          ( minimizeTransactions
              ( netos
                  [ (u3, Monto $ Decimal.Decimal 2 -3)
                  ]
              )
          )
          `shouldThrow` errorCall "Balanace is not 0, instead is: -1.0"
    context "cuando hay muchos participantes" $ do
      it "simplifica en un tiempo razonable algo que mas o menos esta bien" $ do
        minimizeTransactions
          ( netos
              [ (participante 1, mkMonto 2 -3200525)
              , (participante 2, mkMonto 2 4300475)
              , (participante 3, mkMonto 2 5235379)
              , (participante 4, mkMonto 2 1503591)
              , (participante 5, mkMonto 2 10034014)
              , (participante 6, mkMonto 2 -2439410)
              , (participante 7, mkMonto 2 -1597765)
              , (participante 8, mkMonto 2 -1496410)
              , (participante 9, mkMonto 2 -3864071)
              , (participante 10, mkMonto 2 -1171859)
              , (participante 11, mkMonto 2 -1171860)
              , (participante 12, mkMonto 2 -405602)
              , (participante 13, mkMonto 2 -867277)
              , (participante 14, mkMonto 2 -405601)
              , (participante 15, mkMonto 2 -1070332)
              , (participante 16, mkMonto 2 -780407)
              , (participante 17, mkMonto 2 -765749)
              , (participante 18, mkMonto 2 -1070334)
              , (participante 19, mkMonto 2 -766257)
              ]
          )
          `shouldSatisfy` ((== 18) . length)

    -- xit "manual testing" $ do
    --   let
    --     grupo :: Grupo
    --     grupo = case fromJSON @Grupo [aesonQQ|
    --         |] of
    --           Success x -> x
    --           Error _ -> error "no json"

    --   minimizeTransactions (calcularNetosTotales grupo) `shouldBe` []
    context "cuando hay netos con coma y numeros decimales extraños" $ do
      it "una deuda simple con montos con mas de dos decimales" $ do
        minimizeTransactions
          ( netos
              [ (u1, mkMonto 10 5)
              , (u2, mkMonto 10 -5)
              ]
          )
          `shouldBe` [ Transaccion Nothing u2 u1 $ mkMonto 10 5
                     ]

      it "una deuda simple con montos heterogeneos" $ do
        minimizeTransactions
          ( netos
              [ (u1, mkMonto 10 5)
              , (u2, mkMonto 10 -5)
              , (u3, mkMonto 0 5)
              , (u4, mkMonto 0 -5)
              ]
          )
          `shouldBe` [ Transaccion Nothing u2 u1 $ mkMonto 10 5
                     , Transaccion Nothing u4 u3 5
                     ]
