module BackgroundJobs (jobsWorkerInit, BgJobs (..)) where

-- This example is using these functions to introduce an artificial delay of a
-- few seconds in one of the jobs. Otherwise it is not really needed.

-- This example is using these functions to introduce an artificial delay of a
-- few seconds in one of the jobs. Otherwise it is not really needed.
import Colog (LogAction, (<&))
import Config qualified
import Data.Aeson as Aeson
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text, trimming)
import OddJobs.ConfigBuilder (mkConfig)
import OddJobs.Job (ConcurrencyControl (..), Job (..), startJobRunner, throwParsePayload)
import Pkg.Mail
import Relude

data BgJobs
  = InviteUserToProject Users.UserId Projects.ProjectId Text Text
  | CreatedProjectSuccessfully Users.UserId Projects.ProjectId Text Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

jobsRunner :: Pool Connection -> LogAction IO String -> Config.EnvConfig -> Job -> IO ()
jobsRunner dbPool logger cfg job =
  throwParsePayload job >>= \case
    InviteUserToProject userId projectId reciever projectTitle' ->
      let projectTitle = projectTitle'
          projectIdTxt = Projects.projectIdText projectId
          subject = [text| You've been invited to a project '$projectTitle' on apitoolkit.io |]
          body =
            toLText
              [trimming|
Hi,<br/>

<p>You have been invited to the $projectTitle project on apitoolkit. 
Please use the following link to activate your account and access the $projectTitle project.</p>
<a href="https://app.apitoolkit.io/p/$projectIdTxt">Click Here</a>
<br/><br/>
Regards,
Apitoolkit team
          |]
       in sendEmail cfg reciever subject body
    CreatedProjectSuccessfully userId projectId reciever projectTitle' ->
      let projectTitle = projectTitle'
          projectIdTxt = Projects.projectIdText projectId
          subject = [text| Project created successfully '$projectTitle' on apitoolkit.io |]
          body =
            toLText
              [trimming|
Hi,<br/>

<p>You have been invited to the $projectTitle project on apitoolkit. 
Please use the following link to activate your account and access the $projectTitle project.</p>
<a href="app.apitoolkit.io/p/$projectIdTxt">Click Here to access the project</a>
<br/><br/>
Regards,<br/>
Apitoolkit team
          |]
       in sendEmail cfg reciever subject body

jobsWorkerInit :: Pool Connection -> LogAction IO String -> Config.EnvConfig -> IO ()
jobsWorkerInit dbPool logger envConfig = startJobRunner $ mkConfig jobLogger "background_jobs" dbPool (MaxConcurrentJobs 1) (jobsRunner dbPool logger envConfig) id
  where
    jobLogger logLevel logEvent = logger <& show (logLevel, logEvent)
