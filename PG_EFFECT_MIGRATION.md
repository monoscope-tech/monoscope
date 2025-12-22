# Migration: pg-transact + pg-transact-effectful → effectful-postgresql

## Overview

Replace `pg-transact` and `pg-transact-effectful` with `effectful-postgresql`. **Keep `pg-entity` as a query builder** using its underscore-prefixed functions (`_select`, `_selectWhere`, `_insert`, etc.) which generate `Query` values.

## Key Transformations

| Current | New |
|---------|-----|
| `DB` effect | `WithConnection` effect |
| `dbtToEff $ query ...` | `query ...` (lifted directly) |
| `dbtToEff $ insert @E val` | `execute (_insert @E) val` |
| `dbtToEff $ selectById x` | `query (_selectWhere @E [[field| id |]]) x` |
| `dbtToEff $ selectOneByField f x` | `query (_selectWhere @E [f]) x` |
| `dbtToEff $ selectManyByField f x` | `query (_selectWhere @E [f]) x` |
| `runDB pool` | `runWithConnectionPool pool` |
| `getPool` | `withConnection \conn -> ...` |

## pg-entity Query Builder Functions

Keep using pg-entity for type-safe query generation:

```haskell
-- SELECT queries
_select @User                      -- "SELECT ... FROM users.users"
_selectWhere @User [[field| id |]] -- "SELECT ... FROM users.users WHERE id = ?"
_selectWithFields @User [f1, f2]   -- "SELECT f1, f2 FROM users.users"

-- INSERT
_insert @User                      -- "INSERT INTO users.users (...) VALUES (...)"

-- UPDATE
_update @User                      -- "UPDATE users.users SET ... WHERE ..."
_updateBy @User [[field| id |]]    -- "UPDATE users.users SET ... WHERE id = ?"

-- DELETE
_delete @User                      -- "DELETE FROM users.users WHERE ..."
_deleteBy @User [[field| id |]]    -- "DELETE FROM users.users WHERE id = ?"
```

## Files to Modify

### Phase 1: Core Infrastructure

1. **monoscope.cabal**
   - Remove: `pg-transact`, `pg-transact-effectful`
   - Keep: `pg-entity` (for query building)
   - Add: `effectful-postgresql`

2. **src/System/Types.hs**
   - Replace `DB` with `WithConnection` in `CommonWebEffects`
   - Replace `Labeled "timefusion" DB` with `Labeled "timefusion" WithConnection`
   - Replace `import Effectful.PostgreSQL.Transact.Effect (DB, runDB)` with `import Effectful.PostgreSQL.Connection.Pool (WithConnection, runWithConnectionPool)`
   - Replace `runDB env.pool` with `runWithConnectionPool env.pool`

### Phase 2: Model Layer

Update imports and function signatures:

```haskell
-- BEFORE
import Database.PostgreSQL.Entity (insert, selectById, selectOneByField)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

insertUser :: DB :> es => User -> Eff es ()
insertUser user = dbtToEff $ insert @User user

userById :: DB :> es => UserId -> Eff es (Maybe User)
userById userId = dbtToEff $ selectById (Only userId)

-- AFTER
import Database.PostgreSQL.Entity (Entity, _insert, _selectWhere)
import Database.PostgreSQL.Entity.Types (field)
import Effectful.PostgreSQL.Connection.Pool (WithConnection)
import Effectful.PostgreSQL.Simple (execute, query)

insertUser :: (WithConnection :> es, IOE :> es) => User -> Eff es ()
insertUser user = void $ execute (_insert @User) user

userById :: (WithConnection :> es, IOE :> es) => UserId -> Eff es (Maybe User)
userById userId = listToMaybe <$> query (_selectWhere @User [[field| id |]]) (Only userId)
```

**Model files to update:**
- `src/Models/Users/Users.hs`
- `src/Models/Users/Sessions.hs`
- `src/Models/Projects/Projects.hs`
- `src/Models/Projects/ProjectApiKeys.hs`
- `src/Models/Projects/ProjectMembers.hs`
- `src/Models/Projects/Dashboards.hs`
- `src/Models/Apis/Issues.hs`
- `src/Models/Apis/Monitors.hs`
- `src/Models/Apis/Anomalies.hs`
- `src/Models/Apis/Endpoints.hs`
- `src/Models/Apis/Shapes.hs`
- `src/Models/Apis/Reports.hs`
- `src/Models/Apis/RequestDumps.hs`
- `src/Models/Apis/Slack.hs`
- `src/Models/Apis/Fields/Types.hs`
- `src/Models/Apis/Fields/Facets.hs`
- `src/Models/Telemetry/Telemetry.hs`

### Phase 3: Page/Handler Layer

Replace `dbtToEff` blocks:

```haskell
-- BEFORE
dbtToEff do
  ProjectApiKeys.insertProjectApiKey pApiKey
  ProjectApiKeys.projectApiKeysByProjectId pid

-- AFTER (if model functions updated)
ProjectApiKeys.insertProjectApiKey pApiKey
ProjectApiKeys.projectApiKeysByProjectId pid

-- OR inline with effectful-postgresql
execute (_insert @ProjectApiKey) pApiKey
query (_selectWhere @ProjectApiKey [[field| project_id |]]) (Only pid)
```

**Page files to update:**
- `src/Pages/Api.hs`
- `src/Pages/Projects.hs`
- `src/Pages/Dashboards.hs`
- `src/Pages/Monitors.hs`
- `src/Pages/Monitors/Testing.hs`
- `src/Pages/Anomalies.hs`
- `src/Pages/Reports.hs`
- `src/Pages/Endpoints/ApiCatalog.hs`
- `src/Pages/LogExplorer/Log.hs`
- `src/Pages/Bots/Slack.hs`
- `src/Pages/Bots/Discord.hs`
- `src/Pages/Bots/Whatsapp.hs`
- `src/Pages/LemonSqueezy.hs`
- `src/Pages/Replay.hs`
- `src/Pages/Share.hs`
- `src/Pages/Onboarding/Onboarding.hs`

### Phase 4: Background Jobs & Processing

- `src/BackgroundJobs.hs`
- `src/ProcessMessage.hs`
- `src/Opentelemetry/OtlpServer.hs`

### Phase 5: Test Infrastructure

- `src/Pkg/TestUtils.hs` - Update `runTestEffect`, `testSessionHeader`, etc.
- `src/Utils.hs` - Update `checkFreeTierExceeded`
- `src/Devel.hs`
- Test files in `test/integration/`

## Import Changes Summary

```haskell
-- REMOVE these imports:
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff, runDB, getPool)
import Database.PostgreSQL.Transact (DBT)
import Database.PostgreSQL.Entity (insert, selectById, selectOneByField, selectManyByField)
import Database.PostgreSQL.Entity.DBT (withPool, execute, query, queryOne)

-- ADD these imports:
import Effectful.PostgreSQL.Connection.Pool (WithConnection, runWithConnectionPool)
import Effectful.PostgreSQL.Simple (execute, execute_, query, query_, withConnection, withTransaction)

-- KEEP these imports (for query building):
import Database.PostgreSQL.Entity (Entity, _insert, _select, _selectWhere, _update, _updateBy, _delete, _deleteBy)
import Database.PostgreSQL.Entity.Types (field, Field, Schema, TableName, PrimaryKey, FieldModifiers, CamelToSnake, GenericEntity)
```

## Effect Constraint Changes

```haskell
-- BEFORE
someFunction :: DB :> es => Args -> Eff es Result

-- AFTER
someFunction :: (WithConnection :> es, IOE :> es) => Args -> Eff es Result
```

## Helper Pattern for Cache Operations

```haskell
-- BEFORE (with getPool)
getProjectIdByApiKey projectKey = do
  pool <- getPool
  appCtx <- ask @AuthContext
  liftIO $ Cache.fetchWithCache appCtx.projectKeyCache projectKey \_ ->
    withPool pool $ queryOne q (Only projectKey)

-- AFTER (with withConnection)
getProjectIdByApiKey projectKey = do
  appCtx <- ask @AuthContext
  withConnection \conn -> liftIO $
    Cache.fetchWithCache appCtx.projectKeyCache projectKey \_ ->
      PG.query conn q (Only projectKey) <&> listToMaybe
```

## Migration Order

1. Update `monoscope.cabal` dependencies
2. Update `src/System/Types.hs` (effect definitions and runners)
3. Migrate model files one by one (start with Users.hs)
4. Migrate page/handler files
5. Migrate background jobs
6. Update test infrastructure
7. Run tests and fix issues

## Notes

- `WithConnection` requires `IOE :> es` in addition to the effect constraint
- For transactions: use `withTransaction` from effectful-postgresql
- Raw connection access: use `withConnection \conn -> liftIO $ PG.query conn ...`
- `Labeled "timefusion" WithConnection` works the same way as before
- pg-entity's underscore functions return `Query` which works directly with effectful-postgresql's `query`/`execute`
