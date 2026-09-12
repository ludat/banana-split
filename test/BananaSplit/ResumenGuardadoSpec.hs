-- | El decoder del jsonb donde se guarda la parte no sumable del resumen de un
-- gasto.
--
-- Tiene su propio spec, y fuera de @Persistence/@ para que no pague la base,
-- porque su contrato es más delicado que el de un derivado genérico: degrada
-- campo por campo en vez de fallar entero. De eso depende que un cambio de
-- formato no se lleve puesto lo que sí se entiende.
module BananaSplit.ResumenGuardadoSpec (
  spec,
) where

import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Protolude
import Test.Hspec

import BananaSplit.Deudas (ErrorResumen (..), TipoErrorResumen (..))
import BananaSplit.Monto (mkMonto)
import BananaSplit.Persistence.ResumenGuardado (ResumenGuardado (..), sinCalcular)

spec :: Spec
spec = describe "ResumenGuardado" $ do
  describe "lo que se guardó se puede volver a leer" $ do
    it "un gasto válido que no es repartija" $
      ida ResumenGuardado{errores = [], participantesEnRepartija = Nothing}

    it "un gasto inválido con un error sin argumentos" $
      ida
        ResumenGuardado
          { errores = [ErrorResumen{objeto = ["deudores"], tipo = ErrorRepartijaSinClaims}]
          , participantesEnRepartija = Just 3
          }

    it "un gasto inválido con un error con montos adentro" $
      ida
        ResumenGuardado
          { errores =
              [ ErrorResumen
                  { objeto = ["pagadores"]
                  , tipo = ErrorPartesTotalNoCoincide (mkMonto 2 1000) (mkMonto 2 2000)
                  }
              ]
          , participantesEnRepartija = Nothing
          }

  -- Lo que hace falta que ande para que un cambio de formato no sea un
  -- apagón: cada campo ilegible cae solo, sin llevarse los demás.
  describe "degrada campo por campo" $ do
    it "un campo que todavía no existía queda en su default" $
      leer (object ["errores" .= ([] :: [ErrorResumen])])
        `shouldBe` Just ResumenGuardado{errores = [], participantesEnRepartija = Nothing}

    it "un campo con el tipo cambiado no se lleva a los otros" $
      leer (object ["errores" .= ([] :: [ErrorResumen]), "participantesEnRepartija" .= ("tres" :: Text)])
        `shouldBe` Just ResumenGuardado{errores = [], participantesEnRepartija = Nothing}

    it "errores ilegible deja el gasto sin calcular pero conserva el resto" $
      leer (object ["errores" .= ("cualquier cosa" :: Text), "participantesEnRepartija" .= (3 :: Int)])
        `shouldBe` Just ResumenGuardado{errores = sinCalcular, participantesEnRepartija = Just 3}

    it "un error con un constructor que ya no existe también" $
      leer
        ( object
            [ "errores" .= [object ["objeto" .= ([] :: [Text]), "tipo" .= object ["ErrorDeUnaVersionFutura" .= ([] :: [Value])]]]
            , "participantesEnRepartija" .= (2 :: Int)
            ]
        )
        `shouldBe` Just ResumenGuardado{errores = sinCalcular, participantesEnRepartija = Just 2}

    it "errores ausente es lo mismo que ilegible: no sabemos si cierra" $
      leer (object ["participantesEnRepartija" .= (1 :: Int)])
        `shouldBe` Just ResumenGuardado{errores = sinCalcular, participantesEnRepartija = Just 1}

    it "un campo que ya no usamos se ignora" $
      leer (object ["errores" .= ([] :: [ErrorResumen]), "unCampoViejo" .= ("algo" :: Text)])
        `shouldBe` Just ResumenGuardado{errores = [], participantesEnRepartija = Nothing}

  -- El único caso donde no hay nada que rescatar, y por eso sí falla entero.
  describe "falla entero sólo si no es un objeto" $ do
    it "un string" $
      leer (String "un formato que ya no existe") `shouldBe` Nothing

    it "una lista" $
      leer (toJSON ([] :: [Int])) `shouldBe` Nothing

    it "null" $
      leer Null `shouldBe` Nothing

-- | Codificar y volver a leer tiene que dar lo mismo.
ida :: ResumenGuardado -> Expectation
ida guardado = decode (encode guardado) `shouldBe` Just guardado

leer :: Value -> Maybe ResumenGuardado
leer = decode . encode
