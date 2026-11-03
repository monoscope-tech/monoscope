-- | Every public @/api/v1@ operation, declared once. The MCP registry takes its
-- tool names, agent access and descriptions from here, and the CLI derives its
-- resource subcommands (parser and dispatch) from each operation's 'cli' entries.
module Web.Operations (Operation (..), Method (..), Verb (..), stdMethod, Access (..), Cli (..), Listing (..), Column (..), Style (..), Arg (..), Positional (..), Target (..), Option (..), Wire (..), Need (..), Kind (..), operations, pathParams, listingOf) where

import Data.Text qualified as T
import Network.HTTP.Types qualified as HT
import Relude


-- | Agent access class; the server maps it onto its permission model.
data Access = View | AdminView | Edit | Admin | PrivateAdmin
  deriving stock (Eq, Show)


data Method = GET | DELETE | Write Verb
  deriving stock (Eq)


-- | Methods that send a JSON body.
data Verb = POST | PUT | PATCH
  deriving stock (Eq)


stdMethod :: Method -> HT.StdMethod
stdMethod = \case
  GET -> HT.GET
  DELETE -> HT.DELETE
  Write POST -> HT.POST
  Write PUT -> HT.PUT
  Write PATCH -> HT.PATCH


data Operation = Operation {name :: Text, method :: Method, path :: Text, access :: Maybe Access, summary :: Text, cli :: [Cli]}


-- | Path parameters become positionals before 'args'; the first is optional and
-- chosen from the resource's 'Listing' when omitted.
data Cli = Cli {command :: NonEmpty Text, args :: [Arg], listing :: Maybe Listing}


-- | The table a collection GET renders; 'idField' leads it and is what the picker returns.
data Listing = Listing {idField :: Text, columns :: [Column]}


-- | Dotted field paths, the first non-empty one shown.
data Column = Column {header :: Text, fields :: NonEmpty Text, style :: Style}


-- | 'State' colours severities and monitor/incident states; 'Active' renders an @active@ flag.
data Style = Brand | Muted | Numeric | Plain | State | Active
  deriving stock (Eq)


data Arg = Pos Positional | Opt Option


data Positional = Positional {meta :: Text, help :: Text, target :: Target}


-- | A string body field, the whole body from a YAML/JSON file, or files to apply one by one.
data Target = BodyText Text | BodyFile | ApplyFiles


-- | Names of one character are short options.
data Option = Option {names :: NonEmpty Text, need :: Need, meta :: Text, help :: Text, wire :: Wire, kind :: Kind}


data Wire = Query Text | Body Text


data Need = Required | Optional


data Kind = Str | Int | Switch | Ids | IntIds


-- | >>> pathParams "/dashboards/{dashboard_id}/widgets/{widget_id}"
-- ["dashboard_id","widget_id"]
pathParams :: Text -> [Text]
pathParams = mapMaybe (T.stripSuffix "}" <=< T.stripPrefix "{") . T.splitOn "/"


-- | A resource's CLI name, listing path and table, by its first path segment.
--
-- >>> (\(name, path, l) -> (name, path, l.idField)) <$> listingOf "members"
-- Just ("members","/members","user_id")
-- >>> void (listingOf "project")
-- Nothing
listingOf :: Text -> Maybe (Text, Text, Listing)
listingOf segment = listToMaybe [(head c.command, o.path, l) | o <- operations, o.method == GET, o.path == "/" <> segment, c <- o.cli, Just l <- [c.listing]]


operations :: [Operation]
operations =
  [ op "get_ingestion_key" GET "/ingestion-key" (Just AdminView) "Get the project's telemetry ingestion key" []
  , op "list_events" GET "/events" (Just View) "Search events (logs, spans) with a KQL query over a time range" []
  , op "get_event_context" GET "/events/{event_id}/time/{timestamp}" (Just View) "Get one event by id at its timestamp" []
  , op "search_events" (Write POST) "/events/query" (Just View) "Search events with a KQL query in a JSON body" []
  , op "query_metrics" GET "/metrics" (Just View) "Run a KQL metrics query" []
  , op "list_metrics" GET "/metrics/catalog" (Just View) "List known metrics with their types and services" []
  , op "get_schema" GET "/schema" (Just View) "Get the telemetry field schema" []
  , op "list_facets" GET "/facets" (Just View) "Get the top values of faceted fields" []
  , op "submit_rrweb_event" (Write POST) "/rrweb" Nothing "Submit browser session replay events" []
  , -- Monitors
    op "list_monitors" GET "/monitors" (Just View) "List monitors" [list "monitors list" [] "id" [col "state" "current_status" State, col "title" "alert_config.title" Brand, col "threshold" "alert_threshold" Numeric, col "last_triggered" "alert_last_triggered" Muted]]
  , op "get_monitor" GET "/monitors/{monitor_id}" (Just View) "Get a single monitor" [at "monitors get" []]
  , op "create_monitor" (Write POST) "/monitors" (Just Edit) "Create monitor from YAML/JSON file" [at "monitors create" [file]]
  , op "update_monitor" (Write PUT) "/monitors/{monitor_id}" (Just Edit) "Replace (PUT) monitor from file" [at "monitors update" [file]]
  , op "patch_monitor" (Write PATCH) "/monitors/{monitor_id}" (Just Edit) "Patch monitor fields from file" [at "monitors patch" [file]]
  , op "apply_monitor" (Write POST) "/monitors/apply" (Just Edit) "Upsert monitor(s) from file or directory, keyed on title" [at "monitors apply" [applyFiles]]
  , op "get_monitor_yaml" GET "/monitors/{monitor_id}/yaml" (Just View) "Dump monitor as applyable YAML" [at "monitors yaml" []]
  , op "delete_monitor" DELETE "/monitors/{monitor_id}" (Just Admin) "Delete a monitor" [at "monitors delete" []]
  , op "mute_monitor" (Write POST) "/monitors/{monitor_id}/mute" (Just Edit) "Mute a monitor" [at "monitors mute" [forMinutes "Mute for N minutes"]]
  , op "unmute_monitor" (Write POST) "/monitors/{monitor_id}/unmute" (Just Edit) "Unmute a monitor" [at "monitors unmute" []]
  , op "resolve_monitor" (Write POST) "/monitors/{monitor_id}/resolve" (Just Edit) "Resolve a monitor" [at "monitors resolve" []]
  , op "toggle_monitor_active" (Write POST) "/monitors/{monitor_id}/toggle_active" (Just Edit) "Toggle active state" [at "monitors toggle-active" []]
  , op "bulk_monitors" (Write POST) "/monitors/bulk" (Just Admin) "Run bulk action over multiple monitors" [at "monitors bulk" $ bulk "delete|activate|deactivate|mute|unmute|resolve" [flag "duration" "MINUTES" "For mute action" (Body "duration_minutes") Int]]
  , -- Dashboards
    op "list_dashboards" GET "/dashboards" (Just View) "List dashboards" [list "dashboards list" [] "id" [self "title" Brand, col "updated" "updated_at" Muted]]
  , op "get_dashboard" GET "/dashboards/{dashboard_id}" (Just View) "Get a single dashboard" [at "dashboards get" []]
  , op "create_dashboard" (Write POST) "/dashboards" (Just Edit) "Create dashboard from YAML/JSON file" [at "dashboards create" [file]]
  , op "update_dashboard" (Write PUT) "/dashboards/{dashboard_id}" (Just Edit) "Replace (PUT) dashboard from file" [at "dashboards update" [file]]
  , op "patch_dashboard" (Write PATCH) "/dashboards/{dashboard_id}" (Just Edit) "Patch dashboard fields from file" [at "dashboards patch" [file]]
  , op "delete_dashboard" DELETE "/dashboards/{dashboard_id}" (Just Admin) "Delete a dashboard" [at "dashboards delete" []]
  , op "star_dashboard" (Write POST) "/dashboards/{dashboard_id}/star" (Just Edit) "Star a dashboard" [at "dashboards star" []]
  , op "unstar_dashboard" DELETE "/dashboards/{dashboard_id}/star" (Just Edit) "Unstar a dashboard" [at "dashboards unstar" []]
  , op "duplicate_dashboard" (Write POST) "/dashboards/{dashboard_id}/duplicate" (Just Edit) "Duplicate dashboard" [at "dashboards duplicate" []]
  , op "get_dashboard_yaml" GET "/dashboards/{dashboard_id}/yaml" (Just View) "Dump dashboard as YAML" [at "dashboards yaml" []]
  , op "apply_dashboard" (Write POST) "/dashboards/apply" (Just Edit) "Upsert dashboard(s) from file or directory, keyed on file_path" [at "dashboards apply" [applyFiles]]
  , op "bulk_dashboards" (Write POST) "/dashboards/bulk" (Just Admin) "Run bulk action over multiple dashboards" [at "dashboards bulk" $ bulk "delete" []]
  , op "get_dashboard_data" GET "/dashboards/{dashboard_id}/data" (Just View) "Get a dashboard's widget data resolved server-side" []
  , op "upsert_dashboard_widget" (Write PUT) "/dashboards/{dashboard_id}/widgets" (Just Edit) "Insert or update a widget on a dashboard" [at "dashboards widget upsert" [file]]
  , op "delete_dashboard_widget" DELETE "/dashboards/{dashboard_id}/widgets/{widget_id}" (Just Admin) "Delete a widget from a dashboard" [at "dashboards widget delete" []]
  , op "reorder_dashboard_widgets" (Write PATCH) "/dashboards/{dashboard_id}/widgets/order" (Just Edit) "Reorder widgets via {id: {x,y,w,h}} JSON/YAML" [at "dashboards widget reorder" [file, flag "tab" "TAB" "Tab slug" (Query "tab") Str]]
  , -- API keys
    op "list_api_keys" GET "/api_keys" (Just AdminView) "List API keys" [list "api-keys list" [] "id" [self "title" Brand, Column "status" ("active" :| ["status"]) Active, col "created" "created_at" Muted]]
  , op "create_api_key" (Write POST) "/api_keys" (Just PrivateAdmin) "Create API key; prints plaintext once" [at "api-keys create" [Pos (Positional "TITLE" "Key title" (BodyText "title"))]]
  , op "get_api_key" GET "/api_keys/{key_id}" (Just AdminView) "Get API key" [at "api-keys get" []]
  , op "delete_api_key" DELETE "/api_keys/{key_id}" (Just PrivateAdmin) "Delete an API key" [at "api-keys delete" []]
  , op "activate_api_key" (Write POST) "/api_keys/{key_id}/activate" (Just PrivateAdmin) "Activate an API key" [at "api-keys activate" []]
  , op "deactivate_api_key" (Write POST) "/api_keys/{key_id}/deactivate" (Just PrivateAdmin) "Deactivate an API key (soft)" [at "api-keys deactivate" [], at "api-keys revoke" []]
  , -- Identity + project
    op "whoami" GET "/me" (Just View) "Show current project identity" [at "me" []]
  , op "get_project" GET "/project" (Just View) "Show the current project" [at "project get" []]
  , op "patch_project" (Write PATCH) "/project" (Just Admin) "PATCH project fields from a YAML/JSON file" [at "project patch" [file]]
  , -- Endpoints
    op "list_endpoints" GET "/endpoints" (Just View) "List endpoints" [list "endpoints list" (paging <> [flag "search" "TEXT" "Substring filter" (Query "search") Str, flag "outgoing" "" "Only outgoing endpoints" (Query "outgoing") Switch]) "id" [self "method" Plain, self "host" Brand, self "url_path" Plain, col "service" "service_name" Muted]]
  , op "get_endpoint" GET "/endpoints/{endpoint_id}" (Just View) "Get endpoint by ID" [at "endpoints get" []]
  , -- Log patterns
    op "list_log_patterns" GET "/log_patterns" (Just View) "List log patterns" [list "log-patterns list" paging "id" [col "count" "occurrence_count" Numeric, col "last" "last_seen_at" Muted, col "pattern" "pattern_hash" Plain]]
  , op "get_log_pattern" GET "/log_patterns/{pattern_id}" (Just View) "Get log pattern by ID" [at "log-patterns get" []]
  , op "ack_log_pattern" (Write POST) "/log_patterns/{pattern_id}/ack" (Just Edit) "Acknowledge log pattern" [at "log-patterns ack" []]
  , op "bulk_log_patterns" (Write POST) "/log_patterns/bulk" (Just Edit) "Run bulk action over multiple log patterns" [at "log-patterns bulk" [action "ack|ignore", ids "Comma-separated pattern IDs" IntIds]]
  , -- Issues
    op "list_issues" GET "/issues" (Just View) "List issues" [list "issues list" (paging <> [flag "status" "STATUS" "open|acknowledged|archived|all" (Query "status") Str, flag "type" "TYPE" "Filter by issue_type" (Query "type") Str, flag "service" "SERVICE" "Filter by service" (Query "service") Str]) "id" [Column "level" ("severity" :| ["level"]) State, col "count" "event_count" Numeric, Column "seen" ("last_seen" :| ["updated_at"]) Muted, Column "title" ("title" :| ["issue_type"]) Plain]]
  , op "get_issue" GET "/issues/{issue_id}" (Just View) "Get issue by ID" [at "issues get" []]
  , op "ack_issue" (Write POST) "/issues/{issue_id}/ack" (Just Edit) "Acknowledge issue \x2014 pauses its notifications" [at "issues ack" [forMinutes "Pause notifications for N minutes (default: until it regresses)"]]
  , op "unack_issue" (Write POST) "/issues/{issue_id}/unack" (Just Edit) "Un-acknowledge issue" [at "issues unack" []]
  , op "archive_issue" (Write POST) "/issues/{issue_id}/archive" (Just Edit) "Archive issue" [at "issues archive" []]
  , op "unarchive_issue" (Write POST) "/issues/{issue_id}/unarchive" (Just Edit) "Unarchive issue" [at "issues unarchive" []]
  , op "bulk_issues" (Write POST) "/issues/bulk" (Just Edit) "Bulk acknowledge/archive issues" [at "issues bulk" $ bulk "acknowledge|unack|archive|unarchive" [flag "for" "MINUTES" "For acknowledge: pause notifications for N minutes" (Body "duration_minutes") Int]]
  , -- Incidents
    op "list_incidents" GET "/incidents" (Just View) "List recent incidents" [list "incidents list" [flag "phase" "PHASE" "active|recovered|resolved" (Query "phase") Str, Opt (Option ("limit" :| ["n"]) Optional "N" "Maximum incidents (default: 20)" (Query "limit") Int)] "id" [self "phase" State, col "started" "started_at" Muted, col "last_event" "last_event_at" Muted]]
  , op "get_incident" GET "/incidents/{incident_id}" (Just View) "Get incident by ID" [at "incidents get" []]
  , -- Teams
    op "list_teams" GET "/teams" (Just View) "List teams" [list "teams list" [] "id" [self "name" Brand, col "members" "member_count" Numeric, col "created" "created_at" Muted]]
  , op "create_team" (Write POST) "/teams" (Just Admin) "Create team from YAML/JSON file" [at "teams create" [file]]
  , op "get_team" GET "/teams/{team_id}" (Just View) "Get a team by ID" [at "teams get" []]
  , op "update_team" (Write PUT) "/teams/{team_id}" (Just Admin) "Replace (PUT) team from file" [at "teams update" [file]]
  , op "patch_team" (Write PATCH) "/teams/{team_id}" (Just Admin) "Patch team fields from file" [at "teams patch" [file]]
  , op "delete_team" DELETE "/teams/{team_id}" (Just Admin) "Delete a team" [at "teams delete" []]
  , op "bulk_teams" (Write POST) "/teams/bulk" (Just Admin) "Run bulk action over multiple teams" [at "teams bulk" $ bulk "delete" []]
  , -- Members
    op "list_members" GET "/members" (Just View) "List project members" [list "members list" [] "user_id" [self "email" Brand, self "first_name" Plain, self "last_name" Plain, self "permission" Plain]]
  , op "add_member" (Write POST) "/members" (Just Admin) "Add a project member (by email or user ID)" [at "members add" [flag "email" "EMAIL" "Email (creates user if missing)" (Body "email") Str, flag "user-id" "UUID" "Existing user ID" (Body "user_id") Str, flag "permission" "PERMISSION" "view|edit|admin (default: view)" (Body "permission") Str]]
  , op "get_member" GET "/members/{user_id}" (Just View) "Get a member by user ID" [at "members get" []]
  , op "patch_member" (Write PATCH) "/members/{user_id}" (Just Admin) "Change a member's permission" [at "members patch" [Pos (Positional "PERMISSION" "view|edit|admin" (BodyText "permission"))]]
  , op "remove_member" DELETE "/members/{user_id}" (Just Admin) "Remove a project member" [at "members remove" []]
  , -- Share links
    op "create_share_link" (Write POST) "/share" (Just Admin) "Create a 48h share link for a stored event" [at "share-link create" [required "event-id" "UUID" "Event ID" "event_id", required "created-at" "ISO8601" "Event created-at timestamp" "event_created_at", flag "type" "TYPE" "request|log|span (default: request)" (Body "event_type") Str]]
  ]
  where
    op name method path access summary cli = Operation{..}
    command c = let (w, rest) = T.breakOn " " c in w :| words rest
    at c args = Cli (command c) args Nothing
    list c args idField columns = Cli (command c) args (Just Listing{..})
    col header field = Column header (field :| [])
    self name = col name name
    option need name meta help wire kind = Opt Option{names = name :| [], ..}
    flag = option Optional
    required name meta help key = option Required name meta help (Body key) Str
    file = Pos (Positional "FILE" "Path to YAML or JSON file" BodyFile)
    applyFiles = Pos (Positional "PATH" "YAML/JSON file or directory" ApplyFiles)
    forMinutes help = flag "for" "MINUTES" help (Query "duration_minutes") Int
    action help = Pos (Positional "ACTION" help (BodyText "action"))
    ids help = option Required "ids" "ID1,ID2,..." help (Body "ids")
    bulk help extra = [action help, ids "Comma-separated UUIDs" Ids] <> extra
    paging = [flag "page" "N" "Page number (0-based, default: 0)" (Query "page") Int, Opt (Option ("limit" :| ["per-page", "n"]) Optional "N" "Items per page (alias: --per-page)" (Query "per_page") Int)]
