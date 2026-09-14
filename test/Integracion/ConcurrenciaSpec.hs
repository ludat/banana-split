-- | Tests que necesitan dos conexiones commiteando de verdad, así que no pueden
-- vivir bajo @test/BananaSplit/Persistence/@: el 'SpecHook' de esa carpeta
-- envuelve cada test en una transacción que se rollbackea, y una transacción no
-- ve lo que otra no commiteó.
--
-- Por eso este módulo maneja sus propias conexiones y deja la base limpia antes
-- de empezar.
module Integracion.ConcurrenciaSpec (
  spec,
) where

import Data.Pool qualified as Pool
import Database.PostgreSQL.Simple (Connection)
import Protolude
import Test.Hspec

import BananaSplit.Persistence qualified as Persistence
import BananaSplit.Seed qualified as Seed
import Site.Config qualified as Config

-- | Cuántas veces se repite la carrera. La ventana es ancha (el recálculo son
-- ~10 queries), así que sin el lock falla en casi todas: con esto alcanza y
-- sobra para detectarlo. Para correr muchas más está
-- @banana-split probar-concurrencia@.
vueltas :: Int
vueltas = 25

spec :: Spec
spec =
  around conConexiones $ do
    -- El oráculo no sabe nada de estas carreras en particular: compara el cache
    -- contra recalcular desde las distribuciones, así que también atraparía
    -- otras formas de dejarlo inconsistente.
    describe "dos personas reclamando en la misma repartija" $
      it "dejan el cache consistente con lo que dicen las distribuciones" $
        \(connA, connB, escenario) ->
          replicateM_ vueltas $ do
            (desdeCache, recalculado) <- Seed.correrVueltaDeClaims connA connB escenario
            desdeCache `shouldBe` recalculado

    describe "alguien editando el gasto mientras otro reclama" $
      it "dejan el cache consistente con lo que dicen las distribuciones" $
        \(connA, connB, escenario) ->
          replicateM_ vueltas $ do
            (desdeCache, recalculado) <- Seed.correrVueltaDeGuardarYClaim connA connB escenario
            desdeCache `shouldBe` recalculado

conConexiones :: ((Connection, Connection, Seed.Escenario) -> IO ()) -> IO ()
conConexiones correr = do
  -- La base ya viene creada, migrada y vacía por el 'SpecHook' de la raíz.
  config <- Config.createConfig "test"
  bracket (Persistence.makePool config) Pool.destroyAllResources $ \pool ->
    -- Anidar dos 'withResource' da dos conexiones distintas —el pool nunca
    -- entrega el mismo recurso a dos tomadores a la vez—, que es justo lo que
    -- la carrera necesita. Es también de donde las saca la app.
    Pool.withResource pool $ \connA ->
      Pool.withResource pool $ \connB -> do
        escenario <- Seed.prepararEscenario connA
        -- Lo que escribe este test queda commiteado, y hay tests que cuentan
        -- filas sin filtrar por grupo: sin esto, el orden entre specs decidiría
        -- si pasan.
        correr (connA, connB, escenario) `finally` Seed.limpiarTodo connA
