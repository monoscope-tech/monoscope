module Pkg.Ortto (mergePerson, mergeOrganization, addToOrganization, pushedTrafficViaSdk) where

import Control.Lens ((.~), (^..), (^?))
import Data.Aeson.Lens (key, nth, values, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.List.Extra (chunksOf)
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Network.Wreq (defaults, header, postWith, putWith, responseBody)
import Relude

pushedTrafficViaSdk :: Text -> [(Projects.ProjectId, Text, Int64, Users.UserId)] -> IO ()
pushedTrafficViaSdk orttoApiKey projectsList = do
  let chunks = chunksOf 50 projectsList -- Limit to 50 items per request
  forM_ chunks \chunk -> do
    let activities =
          chunk & map \(pid, projectName, count, userId) ->
            let pidText = pid.toText
                userIdText = userId.toText
             in [aesonQQ|
                          {
                              "activity_id": "act:cm:pushed-traffic-via-sdk-24hrs",
                              "attributes": {
                                  "int:cm:traffic-count":#{count},
                                  "str:cm:company-id": #{pidText}
                              },
                              "fields": {
                                  "str::ei": #{userIdText}
                              }
                          }
                        |]
    let requestBody =
          [aesonQQ| {
                "async": true,
                "activities": #{activities},
                "merge_by": [
                    "str::ei"
                ]
              }
            |]
    r <-
      postWith
        (defaults & header "X-Api-Key" .~ [encodeUtf8 @Text @ByteString orttoApiKey] & header "Content-Type" .~ ["application/json"])
        "https://api.eu.ap3api.com/v1/activities/create"
        requestBody
    pass
  pass

mergePerson :: Text -> Users.UserId -> Text -> Text -> Text -> IO (Maybe Text)
mergePerson orttoApiKey userId' firstName lastName email = do
  let userId = userId'.toText
  r <-
    postWith
      (defaults & header "X-Api-Key" .~ [encodeUtf8 @Text @ByteString orttoApiKey] & header "Content-Type" .~ ["application/json"])
      "https://api.eu.ap3api.com/v1/person/merge"
      [aesonQQ| {
            "people": [
              { 
                "fields": {
                  "str::ei": #{userId}, 
                  "str::first": #{firstName},
                  "str::last": #{lastName},
                  "str::email": #{email}
                }
              }
            ],
            "async": false,
            "merge_by": [ "str::ei", "str::email" ], 
            "merge_strategy": 2, 
            "find_strategy": 0
          }
      |]
  pure $ r ^? responseBody . key "people" . nth 0 . key "person_id" . _String

mergeOrganization :: Text -> Projects.ProjectId -> Text -> Text -> IO (Maybe Text)
mergeOrganization orttoApiKey pid projectName paymentPlan = do
  let projectIdText = pid.toText
  r <-
    postWith
      (defaults & header "X-Api-Key" .~ [encodeUtf8 @Text @ByteString orttoApiKey] & header "Content-Type" .~ ["application/json"])
      "https://api.eu.ap3api.com/v1/organizations/merge"
      [aesonQQ| {
        "organizations": [
          { 
            "fields": {
              "str:o:name": #{projectName},
              "str:oc:external-id": #{projectIdText},
              "str:oc:payment-plan": #{paymentPlan}
            }
          }
        ],
        "async": false,
        "merge_by": [ "str:oc:external-id" ], 
        "merge_strategy": 2, 
        "find_strategy": 0
      } 
  |]
  pure $ r ^? responseBody . key "organizations" . nth 0 . key "organization_id" . _String

getPersonIDs :: Text -> [Users.UserId] -> IO [Text]
getPersonIDs orttoApiKey userIds' = do
  let userIds = (.toText) <$> userIds'
  let subJSON =
        userIds & map \userId ->
          [aesonQQ|{
                  "$str::is": {
                    "field_id": "str::ei",
                    "value": #{userId}
                  }
                }
        |]

  r <-
    postWith
      (defaults & header "X-Api-Key" .~ [encodeUtf8 @Text @ByteString orttoApiKey] & header "Content-Type" .~ ["application/json"])
      "https://api.eu.ap3api.com/v1/person/get"
      [aesonQQ| {
            "limit": 100, 
            "offset": 0, 
            "fields": ["str::ei"], 
            "filter": { 
              "$or": #{subJSON}
            }
          }
      |]
  pure $ r ^.. responseBody . key "contacts" . values . key "id" . _String

addToOrganization :: Text -> Text -> [Users.UserId] -> IO ()
addToOrganization orttoApiKey pEI userIds = do
  oUIDs <- getPersonIDs orttoApiKey userIds
  r <-
    putWith
      (defaults & header "X-Api-Key" .~ [encodeUtf8 @Text @ByteString orttoApiKey] & header "Content-Type" .~ ["application/json"])
      "https://api.eu.ap3api.com/v1/organizations/contacts/add"
      [aesonQQ| {
            "inclusion_ids": #{oUIDs},
            "organization_id": #{pEI}
          }
      |]

  pass

getOrganization :: Text -> Projects.ProjectId -> IO (Maybe Text)
getOrganization orttoApiKey pid = do
  let projectIdText = pid.toText
  r <-
    postWith
      (defaults & header "X-Api-Key" .~ [encodeUtf8 @Text @ByteString orttoApiKey] & header "Content-Type" .~ ["application/json"])
      "https://api.eu.ap3api.com/v1/organization/get"
      [aesonQQ| {
      "type": "organization",
      "filter": {
        "$str::is": {
          "field_id": "str:o:ei",
          "value": #{projectIdText}
        }
      }
    }
  |]
  pure $ r ^? responseBody . key "organizations" . nth 0 . key "id" . _String
