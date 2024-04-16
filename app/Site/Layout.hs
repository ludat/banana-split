{-# LANGUAGE OverloadedRecordDot #-}

module Site.Layout
    ( navBarItemsForGrupo
    ) where
import BananaSplit

import Lucid

import Servant

import Site.Api

navBarItemsForGrupo :: Monad m => Grupo -> HtmlT m ()
navBarItemsForGrupo grupo = do
  li_ $ a_
    [ class_ "contrast"
    , href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoGet grupo.grupoId
    ] $ toHtml grupo.grupoNombre
