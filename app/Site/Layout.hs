{-# LANGUAGE OverloadedRecordDot #-}

module Site.Layout
  ( navBarItemsForGrupo
  ) where
import BananaSplit
import Lucid
import Site.Api
import Servant

navBarItemsForGrupo :: Monad m => Grupo -> HtmlT m ()
navBarItemsForGrupo grupo = do
  a_
    [ class_ "navbar-item"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoGet grupo.grupoId
    ] $ toHtml grupo.grupoNombre
  a_
    [ class_ "navbar-item"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagosGet grupo.grupoId
    ] "Pagos"
  a_
    [ class_ "navbar-item"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoParticipantesShow grupo.grupoId
    ] "Participantes"