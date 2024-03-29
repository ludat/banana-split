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
  li_ $ a_
    [ class_ "contrast"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoGet grupo.grupoId
    ] $ toHtml grupo.grupoNombre
  li_ $ a_
    [ class_ "contrast"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagosGet grupo.grupoId
    ] "Pagos"
  li_ $ a_
    [ class_ "contrast"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoParticipantesShow grupo.grupoId
    ] "Participantes"