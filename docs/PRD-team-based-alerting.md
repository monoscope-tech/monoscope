# PRD: Team-Based Alerting & Notification System

## Overview

This document outlines the redesigned notification and alerting system with team-centric architecture, clear integration visibility, and an @everyone default team concept.

## Problem Statement

### Current Issues

1. **No default notification fallback** - When no teams are configured on an alert, behavior is unclear
2. **Integration visibility gap** - On `/p/:projectID/integrations`, users cannot see:
   - Which Slack workspace is connected
   - Which Slack/Discord channels are available
   - Which emails are configured for notifications
3. **Slack OAuth 404 bug** - Route expects `/slack/oauth/callback/:project_id` but Slack redirects to `/slack/oauth/callback/?code=...` (missing project_id in path)
4. **Team-channel relationship unclear** - No clear way to see/add notification channels per team
5. **No @everyone concept** - Missing a default team that notifies all project members

## Proposed Solution

### 1. @everyone Meta-Team

Introduce a special `@everyone` team that:

- Is auto-created for every project (cannot be deleted)
- Has `handle = "everyone"`
- Automatically includes all project members
- Inherits all configured integration channels (Slack, Discord, Email, WhatsApp)
- Serves as the default notification target when no team is specified on an alert

```haskell
-- Pseudo-structure for @everyone
everyoneTeam = Team
  { handle = "everyone"
  , name = "Everyone"
  , description = "All project members and configured channels"
  , members = <all active project members>
  , notify_emails = <all member emails + project notifyEmails>
  , slack_channels = <all connected Slack channels>
  , discord_channels = <all connected Discord channels>
  , phone_numbers = <all project whatsappNumbers>
  }
```

### 2. Redesigned Integrations Page (`/p/:projectID/integrations`)

#### Layout

```
┌──────────────────────────────────────────────────────────────────┐
│ Integrations                                                     │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ ℹ️  Channels configured here are available to the @everyone │  │
│ │    team. Alerts targeting @everyone will notify all         │  │
│ │    members and channels below.                              │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│ SLACK                                              [Disconnect]  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ ✓ Connected to: Acme Corp Workspace                         │  │
│ │   Channels: #alerts, #engineering, #on-call                 │  │
│ │   [Manage Channels ▾]                                       │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                       -- OR if not connected --  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ ○ Not connected                                             │  │
│ │   [Add to Slack]                                            │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│ DISCORD                                            [Disconnect]  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ ✓ Connected to: Acme Gaming Server                          │  │
│ │   Channels: #alerts, #monitoring                            │  │
│ │   [Manage Channels ▾]                                       │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│ EMAIL                                                            │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ Notification emails:                                        │  │
│ │ • ops@acme.com (configured)                                 │  │
│ │ • alerts@acme.com (configured)                              │  │
│ │ • john@acme.com (member - auto-included in @everyone)       │  │
│ │ • jane@acme.com (member - auto-included in @everyone)       │  │
│ │                                                             │  │
│ │ [+ Add email]                                               │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│ WHATSAPP / SMS                                                   │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ Phone numbers:                                              │  │
│ │ • +1-555-0123 (ops team)                                    │  │
│ │ • +1-555-0456 (on-call)                                     │  │
│ │                                                             │  │
│ │ [+ Add phone number]                                        │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

#### Key Features

1. **Connection Status** - Show workspace/server name for Slack/Discord
2. **Channel Visibility** - List all connected channels
3. **Email Transparency** - Show both configured emails AND member emails
4. **@everyone Explainer** - Clear banner explaining that @everyone inherits all channels

### 3. Redesigned Teams Page (`/p/:projectID/manage_teams`)

#### Layout

```
┌──────────────────────────────────────────────────────────────────┐
│ Teams                                           [+ Create Team]  │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ @everyone (default team)                           [View]   │  │
│ │ All project members and global integrations                 │  │
│ │                                                             │  │
│ │ Members: 👤👤👤👤 +8 more                                   │  │
│ │ Channels: #alerts #eng │ 📧 4 emails │ 📱 2 phones         │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ @backend-team                              [Edit] [Delete]  │  │
│ │ Backend engineering team                                    │  │
│ │                                                             │  │
│ │ Members: 👤👤👤                                             │  │
│ │ Channels: #backend-alerts │ 📧 2 emails                    │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ @infra                                     [Edit] [Delete]  │  │
│ │ Infrastructure on-call rotation                             │  │
│ │                                                             │  │
│ │ Members: 👤👤                                               │  │
│ │ Channels: #infra │ 📧 1 email │ 📱 1 phone                 │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

#### Team Edit Modal

```
┌──────────────────────────────────────────────────────────────────┐
│ Edit Team: @backend-team                                    [X]  │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│ Name:        [Backend Team                    ]                  │
│ Handle:      [@backend-team                   ]                  │
│ Description: [Backend engineering team        ]                  │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│ MEMBERS                                                          │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ ☑ John Smith (john@acme.com)                                │  │
│ │ ☑ Jane Doe (jane@acme.com)                                  │  │
│ │ ☐ Bob Wilson (bob@acme.com)                                 │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│ NOTIFICATION CHANNELS                                            │
│                                                                  │
│ Slack Channels:                                                  │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ [Select channels...                                    ▾]   │  │
│ │ Available: #alerts, #engineering, #backend-alerts, #general │  │
│ └─────────────────────────────────────────────────────────────┘  │
│ Selected: #backend-alerts                                        │
│                                                                  │
│ Discord Channels:                                                │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ [Select channels...                                    ▾]   │  │
│ │ Available: #alerts, #monitoring                             │  │
│ └─────────────────────────────────────────────────────────────┘  │
│ Selected: (none)                                                 │
│                                                                  │
│ Additional Emails:                                               │
│ [backend-alerts@acme.com] [x]                                    │
│ [+ Add email]                                                    │
│                                                                  │
│ Phone Numbers:                                                   │
│ (none configured)                                                │
│ [+ Add phone]                                                    │
│                                                                  │
│ ─────────────────────────────────────────────────────────────────│
│                                                                  │
│                                        [Cancel]    [Save Team]   │
└──────────────────────────────────────────────────────────────────┘
```

### 4. Alert Configuration

When configuring an alert/monitor:

```
┌──────────────────────────────────────────────────────────────────┐
│ Alert: High Error Rate                                           │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│ Notify Teams:                                                    │
│ ┌─────────────────────────────────────────────────────────────┐  │
│ │ [Select teams...                                       ▾]   │  │
│ │ ☑ @everyone (default - notifies all)                        │  │
│ │ ☐ @backend-team                                             │  │
│ │ ☐ @infra                                                    │  │
│ └─────────────────────────────────────────────────────────────┘  │
│                                                                  │
│ ℹ️  If no team is selected, @everyone will be notified.         │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

### 5. Bug Fix: Slack OAuth 404

**Current Issue:**

- Route: `/slack/oauth/callback/:project_id` (expects project_id in path)
- Slack redirects to: `/slack/oauth/callback/?code=...` (no project_id)

**Root Cause:**
The `slackRedirectUri` env var likely doesn't include the project_id, or the redirect_uri registered in Slack doesn't match.

**Solution Options:**

#### Option A: Use State Parameter (Recommended)

Store project_id in OAuth `state` parameter like Discord does:

```haskell
-- Slack OAuth initiation
slackOAuthUrl = "https://slack.com/oauth/v2/authorize?client_id=..."
  <> "&redirect_uri=" <> envCfg.slackRedirectUri
  <> "&state=" <> projectId  -- encode project_id here

-- Route change
slackLinkProjectGet :: mode :- "slack" :> "oauth" :> "callback"
  :> QPT "code" :> QPT "state" :> ...  -- state contains project_id
```

#### Option B: Cookie/Session Storage

Store project_id in session before OAuth redirect, retrieve after callback.

---

## Data Model Changes

### 1. New: `is_everyone` Flag on Team

```sql
ALTER TABLE projects.teams ADD COLUMN is_everyone BOOLEAN DEFAULT FALSE;

-- Auto-create @everyone for existing projects
INSERT INTO projects.teams (project_id, name, handle, description, is_everyone, ...)
SELECT id, 'Everyone', 'everyone', 'All project members and global integrations', TRUE, ...
FROM projects.projects
WHERE id NOT IN (SELECT project_id FROM projects.teams WHERE is_everyone = TRUE);
```

### 2. Update Team Model

```haskell
data Team = Team
  { id :: UUID.UUID
  , name :: Text
  , handle :: Text
  , description :: Text
  , members :: V.Vector Users.UserId
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  , phone_numbers :: V.Vector Text
  , is_everyone :: Bool  -- NEW
  , created_at :: UTCTime
  , updated_at :: UTCTime
  }
```

### 3. Virtual @everyone Team Computation

```haskell
-- Get or compute @everyone team with inherited channels
getEveryoneTeam :: ProjectId -> IO Team
getEveryoneTeam projectId = do
  project <- getProject projectId
  members <- getProjectMembers projectId
  slackData <- getSlackData projectId
  discordData <- getDiscordData projectId

  pure Team
    { handle = "everyone"
    , name = "Everyone"
    , is_everyone = True
    , members = map (.userId) members
    , notify_emails = project.notifyEmails <> map (.email) members
    , slack_channels = maybe V.empty (V.singleton . (.channelId)) slackData
    , discord_channels = maybe V.empty (V.singleton . fromMaybe "") discordData.notifsChannelId
    , phone_numbers = project.whatsappNumbers
    , ...
    }
```

---

e+pyxVEx6ztgzNjBc63g

## API Changes

### New Endpoints

| Method | Path                                          | Description                                 |
| ------ | --------------------------------------------- | ------------------------------------------- |
| GET    | `/p/:projectID/integrations/status`           | Returns connected integrations with details |
| GET    | `/p/:projectID/integrations/slack/channels`   | List available Slack channels               |
| GET    | `/p/:projectID/integrations/discord/channels` | List available Discord channels             |
| DELETE | `/p/:projectID/integrations/slack`            | Disconnect Slack                            |
| DELETE | `/p/:projectID/integrations/discord`          | Disconnect Discord                          |

### Modified Endpoints

| Method | Path                          | Change                                            |
| ------ | ----------------------------- | ------------------------------------------------- |
| GET    | `/slack/oauth/callback`       | Remove `:project_id` from path, use `state` param |
| GET    | `/p/:projectID/teams`         | Include computed @everyone team                   |
| POST   | `/p/:projectID/teams`         | Prevent creating team with handle "everyone"      |
| DELETE | `/p/:projectID/teams/:handle` | Prevent deleting @everyone team                   |

---

## Notification Dispatch Logic

```haskell
sendAlertNotifications :: Monitor -> [Team] -> Project -> Alert -> IO ()
sendAlertNotifications monitor teams project alert = do
  -- If no teams specified, default to @everyone
  targetTeams <- if null teams
    then pure <$> getEveryoneTeam project.id
    else pure teams

  for_ targetTeams \team -> do
    -- Email all team member emails + additional emails
    for_ team.notify_emails (sendEmail alert)

    -- Slack channels
    for_ team.slack_channels (sendSlackMessage alert project)

    -- Discord channels
    for_ team.discord_channels (sendDiscordMessage alert project)

    -- Phone/WhatsApp
    for_ team.phone_numbers (sendWhatsApp alert)
```

---

## Industry Comparison

| Feature                 | Monoscope (Proposed) | PagerDuty            | Datadog         | Opsgenie      |
| ----------------------- | -------------------- | -------------------- | --------------- | ------------- |
| Default team            | @everyone            | Default service      | Default notify  | All users     |
| Team-based routing      | ✓                    | Escalation policies  | Monitor -> Team | Routing rules |
| Slack channels per team | ✓                    | Service integrations | Per monitor     | Per policy    |
| Discord support         | ✓                    | ✗                    | ✗               | ✗             |
| Email lists             | ✓                    | ✓                    | ✓               | ✓             |
| Integration visibility  | ✓ (enhanced)         | Good                 | Good            | Good          |

**Our Approach Aligns With:**

- PagerDuty's concept of services owning integrations
- Datadog's team-scoped notification channels
- Opsgenie's routing flexibility

**Differentiators:**

- Discord as first-class citizen
- @everyone concept is simpler than escalation policies
- Clear inheritance model (project → @everyone → custom teams)

---

## Migration Plan

### Phase 1: Data Migration

1. Add `is_everyone` column to teams table
2. Create @everyone team for all existing projects
3. Migrate project-level notification settings to @everyone team

### Phase 2: Slack OAuth Fix

1. Update OAuth flow to use state parameter
2. Update route to not require project_id in path
3. Test OAuth flow end-to-end

### Phase 3: UI Updates

1. Redesign integrations page with connection status
2. Add channel selection to team edit modal
3. Update alert configuration to show team selection
4. Add @everyone explainer banners

### Phase 4: Notification Logic

1. Update dispatch logic to use @everyone as fallback
2. Ensure team channels override project-level channels
3. Add logging for notification routing

---

## Success Metrics

1. **Clarity**: Users can see exactly which channels are connected within 5 seconds
2. **Discoverability**: @everyone team is immediately visible on teams page
3. **Reliability**: Slack OAuth success rate > 99%
4. **Adoption**: 80% of alerts have at least one team assigned

---

## Open Questions

1. Should @everyone be editable (add extra channels) or purely computed?
2. Should we support channel inheritance (team inherits from @everyone + has own channels)?
3. Do we need per-user notification preferences (mute teams, DND hours)?
4. Should we add a "test notification" button per team?

---

## Appendix: Current File Locations

| Component             | File                                            |
| --------------------- | ----------------------------------------------- |
| Team Model            | `src/Models/Projects/ProjectMembers.hs:192-431` |
| Slack OAuth           | `src/Pages/Bots/Slack.hs:104-129`               |
| Discord OAuth         | `src/Pages/Bots/Discord.hs:55-80`               |
| Integrations Page     | `src/Pages/Projects.hs:245-434`                 |
| Teams Page            | `src/Pages/Projects.hs:549-771`                 |
| Notification Dispatch | `src/BackgroundJobs.hs:843-862`                 |
| Routes                | `src/Web/Routes.hs:187` (Slack callback)        |
