module Pkg.RouteUtils (QPT , QP, QPU,QPB, QPI, QEID, ProjectId,  GetRedirect ) where

import Relude
import Servant
import Models.Apis.Endpoints qualified as Endpoints
import Data.Time (UTCTime)
import Models.Projects.Projects qualified as Projects

type QPT a = QueryParam a Text


type QP a b = QueryParam a b


type QPU a = QueryParam a UTCTime


type QPB a = QueryParam a Bool


type QPI a = QueryParam a Int


type QEID a = QueryParam a Endpoints.EndpointId


type ProjectId = Capture "projectID" Projects.ProjectId


type GetRedirect = Verb 'GET 302
