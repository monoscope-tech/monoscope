{-# LANGUAGE NamedFieldPuns #-}

module Pages.Projects.ProjectSettings
  (
  )
where

import Config
  ( AuthContext (pool),
    DashboardM,
    HeadersTriggerRedirect,
  )
import Data.Default (Default (def))
import Data.UUID qualified as UUID
import Data.Valor (validateM)
import Data.Valor qualified as Valor
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX (hxPost_, hxTarget_)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Optics.Operators ()
import Optics.TH ()
import Pages.BodyWrapper (bodyWrapper)
import Pages.Projects.CreateProject qualified as CreateProject
import Relude
import Servant
  ( addHeader,
    noHeader,
  )
