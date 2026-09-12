-- | Preparación de la base para los tests que la usan.
--
-- No cuelga de un 'SpecHook' en la raíz a propósito: eso la haría correr
-- también para los specs puros, que hoy no necesitan que haya un Postgres
-- levantado. La invoca cada directorio que sí la precisa.
module BaseDePrueba (
  prepararBase,
) where

import Database.PostgreSQL.Simple (close)
import Protolude

import BananaSplit.Persistence qualified as Persistence
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.Seed qualified as Seed
import Site.Config qualified as Config

-- | Deja la base creada, migrada y vacía.
--
-- Vaciarla acá y no adentro de cada test es lo que hace que una corrida no
-- dependa de la anterior: el test de integración commitea de verdad (necesita
-- dos conexiones que se vean entre sí), así que si se cayera antes de limpiar
-- lo suyo, la siguiente arrancaría sobre esa basura.
--
-- Las tres operaciones son idempotentes, así que no importa que la llamen
-- varios directorios.
prepararBase :: IO ()
prepararBase = do
  config <- Config.createConfig "test"
  Persistence.crearBaseSiNoExiste config
  PgRoll.init config
  PgRoll.startAndComplete config
  bracket (Persistence.openConnection config) close Seed.limpiarTodo
