module Pkg.ConvertKit (addUser, addUserOrganization) where

import Control.Lens ((&), (.~))
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.Wreq
import Data.Text (Text)
import Effectful
import Relude (pass)


-- Function to add user to ConvertKit
addUser :: HTTP :> es => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Eff es ()
addUser apiKey email firstName lastName orgId orgName plan = do
  r <-
    postWith
      (defaults & header "Content-Type" .~ ["application/json"])
      "https://api.convertkit.com/v3/forms/5502985/subscribe"
      [aesonQQ| {
            "api_key": #{apiKey},
            "email": #{email},
            "first_name": #{firstName},
            "fields": {
             "last_name": #{lastName}
            }
          }
        |]
  -- let subscriberId = r ^? responseBody . key "subscription" . key "subscriber" . key "id" . _String
  pass


addUserOrganization :: HTTP :> es => Text -> Text -> Text -> Text -> Text -> Eff es ()
addUserOrganization apiKey email orgID orgName orgPlan = do
  r <-
    postWith
      (defaults & header "Content-Type" .~ ["application/json"])
      "https://api.convertkit.com/v3/tags/4059942/subscribe"
      [aesonQQ| {
          "api_key": #{apiKey},
          "email": #{email},
          "fields": {
            "organization_name": #{orgName},
            "organization_plan": #{orgPlan},
            "organization_id": #{orgID} 
          } 
        }
      |]
  -- traceShowM $ r ^. responseBody
  pass
