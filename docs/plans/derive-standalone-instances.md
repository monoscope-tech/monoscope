# Derive standalone instances

Branch: `typed-handlers`. Scope: every hand-written `instance` in `src/`, `shared/src/`, `cli/CLI/` and `test/` that a `deriving` clause, an existing via-wrapper, or a new small via-wrapper can replace. An audit of all 414 standalone instances found that most contain real logic and stay as they are (see [Not doing](#not-doing)).

**Compatibility rule.** We keep one version of everything. When a derivation changes what is stored, add a migration that rewrites the rows. Do not keep a hand-written codec just to read old data. The exception is a contract held **outside** our database, such as a URL registered at a customer's Git host. We cannot migrate those.

## Order of work

| Phase | What | Net lines | Behaviour change |
|---|---|---|---|
| 1 | Fix the widget PUT empty body (failing test first) and give it a typed `WidgetSaved` outcome | ≈ −5 | Bug fix |
| 2 | Widget PUT: refuse an invalid monitor query, return `WidgetNotFound` instead of an untyped 404 | ≈ +5 | Bug fix |
| 3 | `Says` and `Mime` via-wrappers in `System.Types`, convert 17 + 6 sites | ≈ −40 | None |
| 4 | Direct derivations with existing wrappers | ≈ −30 | Error text only |
| 5 | Template Haskell splice for the single-column `DecodeRow` instances | ≈ −20 | None |
| 6 | Optional: `LatencyHist` via `monoidal-containers` | ≈ −5 | None |

Phases 3–5 edit `System.Types` and `Pkg.DeriveUtils`, which almost every module imports. Make all edits to each of those files and then save once, so ghcid recompiles the project once instead of once per edit.

---

## Phase 1 — Widget PUT returns an empty body

### The bug

`src/Pages/Dashboards.hs:1249` has an empty instance:

```haskell
instance Outcome Widget.Widget
```

`outcomeBody` therefore defaults to `mempty`, so the PUT answers `200` with no body. The form's swap handler (`src/Pages/Dashboards.hs:1617`) reads that body:

```js
const saved = window.adoptDashboardWidget(event.detail.ctx.text, isNew ? null : document.getElementById('${sourceWid}_widgetEl'));
if (configureMonitor && saved) { … }
```

As a result, a saved widget is never added to the grid, and the "Add widget and monitor" flow never reaches the monitor step. The bug arrived in 6e927c551: the old `HxOutcome` class required `ToHtml`, so its `Ok` wrapper rendered the widget.

The handler also chooses its toast inline (`src/Pages/Dashboards.hs:1288`), which is the pattern this branch is meant to remove:

```haskell
addSuccessToast (if isJust existingMonitorM then "Widget and monitor updated successfully" else if isJust normalizedWidgetIdM then "Widget updated successfully" else "Widget added to dashboard successfully") Nothing
```

### Step 1: failing test (`test/integration/Pages/DashboardWidgetsSpec.hs`)

Name the test after the bug. It checks what the browser receives: the response body contains the saved widget's markup, and the toast names the kind of change.

```haskell
it "dashboardWidgetPut_returnsWidgetMarkup" \tr -> do
  dashId <- createDashboard tr
  -- `call` reads back which alternative the action answered with (Settled in Pkg.TestUtils)
  Right saved <- call tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTStat "Body Check")
  let html = renderText (toHtml (Done saved))
  html `shouldSatisfy` T.isInfixOf "Body Check"
  html `shouldSatisfy` T.isInfixOf "data-widget="  -- adoptDashboardWidget parses this attribute
```

Run it alone with `echo '--match "dashboardWidgetPut_returnsWidgetMarkup"' > .test-args` and confirm it fails because the body is empty.

### Step 2: fix with a typed outcome

```haskell
-- | What a widget save did, so the toast is chosen by the type rather than inline.
data WidgetChange = WidgetAdded | WidgetUpdated | WidgetAndMonitorUpdated
  deriving stock (Eq, Show)


data WidgetSaved = WidgetSaved WidgetChange Widget.Widget


instance Outcome WidgetSaved where
  outcomeBody (WidgetSaved _ w) = toHtml w
  outcomeToast (WidgetSaved change _) = Just case change of
    WidgetAdded -> "Widget added to dashboard successfully"
    WidgetUpdated -> "Widget updated successfully"
    WidgetAndMonitorUpdated -> "Widget and monitor updated successfully"
```

Handler changes:

```haskell
dashboardWidgetPutWithMonitorH :: … -> ATAuthCtx (ActionResult WidgetMonitorRejection WidgetSaved)
…
        unless (configureMonitorM == Just "true") $ addTriggerEvent "closeModal" ""
        pure $ WidgetSaved (if isJust existingMonitorM then WidgetAndMonitorUpdated else if isJust normalizedWidgetIdM then WidgetUpdated else WidgetAdded) widgetUpdated
```

Delete `instance Outcome Widget.Widget` and the `addSuccessToast` line. Update the route type in `Web/Routes.hs` and the three callers in `DashboardWidgetsSpec.hs`, which use `dashboardWidgetPutH` and read the widget back as `WidgetSaved _ w`.

`deriving (Outcome) via Rendered Widget` on `Widget` would also fix the empty body. It would leave the toast chosen inline, though, so use `WidgetSaved`.

---

## Phase 2 — Two failures in the widget PUT the types don't show

### 2a. A monitor keeps alerting on its old SQL after its query changes

`src/Pages/Dashboards.hs:1272`:

```haskell
newSqlQuery =
  if monitor.logQuery == newQuery
    then monitor.logQueryAsSql
    else case parseQueryToComponents sqlQueryCfg newQuery of
      Right (_, qc) -> fromMaybe "" qc.finalAlertQuery   -- stores "" in place of real SQL
      Left _ -> monitor.logQueryAsSql                     -- new KQL, old SQL, no signal
```

When the new query does not parse, the monitor stores the new KQL next to the old SQL and keeps alerting on the old query. Nobody is told. The sibling handler `widgetAlertUpsertH` (`:2001`) already refuses this case with `MonitorQueryInvalid`, which this branch added.

**Test first** (`DashboardWidgetsSpec.hs`): attach a monitor to a widget, PUT the widget with an unparsable query, and assert that the result is `Left (MonitorQueryInvalid _)` and that the monitor's `logQuery` and `logQueryAsSql` are unchanged.

**Fix:** work out the monitor update before saving anything, and refuse when it fails.

```haskell
  let monitorSql monitor newQuery
        | monitor.logQuery == newQuery = Right monitor.logQueryAsSql
        | otherwise = parseQueryToComponents (sqlQueryCfgFor monitor) newQuery >>= maybeToRight "query has no alert form" . (.finalAlertQuery) . snd
  case (existingMonitorM, widgetUpdated.alertThreshold) of
    (Just _, Nothing) -> pure $ Left MonitorNeedsThreshold
    (Just monitor, _) | Left err <- monitorSql monitor (fromMaybe monitor.logQuery widget.query) -> pure $ Left $ MonitorQueryInvalid err
    _ -> Right <$> do …   -- the save path, now using the already-validated SQL
```

### 2b. A missing widget is an untyped 404

`src/Pages/Dashboards.hs:1267`:

```haskell
in maybe (throwError err404{errBody = "Widget not found in dashboard"}) …
```

The user can correct this failure (their edit form is out of date), so it belongs in the rejection type:

```haskell
data WidgetMonitorRejection = RawSqlWidget | MissingThreshold | InvalidWarningThreshold | MonitorNeedsThreshold | MonitorQueryInvalid Text | WidgetNotFound

-- in refusalMessage:
    WidgetNotFound -> "This widget is no longer on the dashboard. Reload to see the current version."
```

The tab lookup returns `Left WidgetNotFound` instead of throwing. Add a spec case: a PUT with an unknown `widget_id` answers `Left WidgetNotFound`.

---

## Phase 3 — `Says` and `Mime` wrappers (`src/System/Types.hs`)

### 3a. `Says`: a fixed message attached to the type

```haskell
-- | A fixed message for a type with exactly one constructor and no fields:
-- @deriving (Refusal) via Says "API key not found" ApiKeyNotFound@.
-- The 'Rep' constraint makes adding a second constructor a compile error, not a reused message.
newtype Says (msg :: Symbol) a = Says a


type family IsOneCase (f :: Type -> Type) :: Constraint where
  IsOneCase (D1 _ (C1 _ U1)) = ()
  IsOneCase _ = TypeError ('Text "Says needs a type with exactly one constructor and no fields")


instance (KnownSymbol msg, Generic a, IsOneCase (Rep a)) => Refusal (Says msg a) where
  refusalMessage _ = toText $ symbolVal (Proxy @msg)


instance (KnownSymbol msg, Generic a, IsOneCase (Rep a)) => Outcome (Says msg a) where
  outcomeToast _ = Just $ successToast $ toText $ symbolVal (Proxy @msg)
```

The constraint is there because, without it, `Says` behaves like a catch-all `_ ->` branch. Changing `data EmptyTitle = EmptyTitle` to `EmptyTitle | TitleTooLong` would compile and silently show "Enter a title." for both. With the constraint, the second constructor forces a real instance with a full `case`. Every one of the 17 targets has a single constructor with no fields today.

Each site looks like this before:

```haskell
data ApiKeyNotFound = ApiKeyNotFound
  deriving stock (Eq, Show)


instance Refusal ApiKeyNotFound where refusalMessage ApiKeyNotFound = "API key not found"
```

and after:

```haskell
data ApiKeyNotFound = ApiKeyNotFound
  deriving stock (Eq, Generic, Show)
  deriving (Refusal) via Says "API key not found" ApiKeyNotFound
```

**Refusal sites (11):**

| File:line | Type | Message |
|---|---|---|
| `Pages/Settings.hs:317` | `ApiKeyNotFound` | API key not found |
| `Pages/AIThreads.hs:200` | `EmptyTitle` | Enter a title. |
| `Pages/AIThreads.hs:207` | `UnknownTemplate` | Unknown routine template. |
| `Pages/AIThreads.hs:214` | `NoReviewClasses` | Choose at least one action class for review. |
| `Pages/Projects.hs:376` | `PagerdutyKeyTooShort` | PagerDuty integration key is too short |
| `Pages/Projects.hs:383` | `SlackNotInstalled` | Failed to disconnect Slack |
| `Pages/Projects.hs:1497` | `DemoProjectProtected` | Can't perform this action on the demo project |
| `Pages/Projects.hs:1568` | `SubscriptionLookupFailed` | Something went wrong while fetching subscription id |
| `Pages/Issues.hs:3018` | `UnnamedView` | Name the view |
| `Pages/Onboarding.hs:222` | `NoEventsYet` | No events found yet |
| `Pages/Dashboards.hs:404` | `InvalidViewName` | View name must contain 1–100 characters. (a type-level Symbol holds the en dash fine) |

**Outcome sites (6):**

| File:line | Type | Toast |
|---|---|---|
| `Pages/Reports.hs:339` | `ReportsPost` | Report notifications updated successfully |
| `Pages/AIThreads.hs:886` | `PolicySaved` | Execution policy saved. |
| `Pages/Projects.hs:1247` | `MemberRemoved` | Member removed |
| `Pages/Issues.hs:3150` | `PatternUnmerged` | Pattern unmerged |
| `Pages/Dashboards.hs:1928` | `WidgetMonitorResult` (`WidgetMonitorSaved`) | Widget monitor configured successfully |
| `Pages/Dashboards.hs:2011` | `WidgetMonitorRemoved` | Monitor removed from widget |

Two cleanups along the way:
- `PatternUnmerged` has no `deriving` clause today, so add `deriving stock (Generic)`.
- Rename `WidgetMonitorResult`/`WidgetMonitorSaved` to one name, `WidgetMonitorSaved`, so the type and its constructor match like the other 16.

**Message checks to keep.** Some specs assert the toast or refusal text through the `HX-Trigger` header. Grep `test/` for each message before converting; the derived text must be byte-identical. That is a Haskell string literal copied into a type-level string. Watch for escapes: a `'` is fine and a `"` would need `\"`. None of the 17 contain one.

**Later, if needed: a fixed toast with a body.** Rendered outcomes with a constant toast could use `Says msg (Rendered a)`, if `Says` delegated `outcomeBody` and `outcomeRedirect` to its inner type. No site needs that today: `ProjectDeleted` has a redirect plus a toast and is the only near miss. Add it when a second case appears.

### 3b. `Mime`: a content type as a type-level string

```haskell
-- | A Servant content type named by its MIME string: @data PNG deriving (Accept) via Mime "image/png"@.
data Mime (ct :: Symbol)


instance KnownSymbol ct => Accept (Mime ct) where
  contentType _ = fromString $ symbolVal (Proxy @ct)
```

This works because `Accept`'s method only mentions `Proxy ctype`, which can be coerced (its parameter is phantom). `RawForm` already uses this shape: `data RawForm deriving (Accept) via FormUrlEncoded`.

Before:

```haskell
data PNG


instance Accept PNG where
  contentType _ = "image/png"
```

After:

```haskell
data PNG
  deriving (Accept) via Mime "image/png"
```

| Site | MIME |
|---|---|
| `Web/Routes.hs:159` `RawJSON` | `application/json` |
| `Web/Routes.hs:180` `OTLPProto` | `application/x-protobuf` |
| `Web/Routes.hs:200` `PNG` | `image/png` |
| `Web/Routes.hs:213` `YAML` | `application/yaml` |
| `Pages/LogExplorer/LiveTail.hs:67` `EventStream` | `text/event-stream` |
| `Pages/Charts/Charts.hs:848` `ChartStream` | `application/x-ndjson` |

`Mime` lives in `System.Types` and not in `Web.Routes`, because `Routes` imports `LiveTail` and `Charts`. `Charts.hs` does not import `System.Types` yet, so add that import.

Do not use `RawJSON via JSON`. Servant's `JSON` also accepts `application/json;charset=utf-8`, which would change how requests are matched.

The `MimeRender` and `MimeUnrender` instances stay. `deriving via` acts on a class's last parameter, which there is the payload type, not the content type.

---

## Phase 4 — Direct derivations with existing wrappers

### 4a. `Pkg/DeriveUtils.hs`: `AesonText` (3 instances become one clause)

Before:

```haskell
newtype AesonText a = AesonText a
  deriving (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)

instance AE.ToJSON a => ToField (AesonText a) where
  toField (AesonText v) = toField (Aeson v)

instance AE.FromJSON a => HI.DecodeValue (AesonText a) where
  decodeValue = coerce (HI.decodeValue @(HI.AsJsonb a))

instance AE.ToJSON a => HI.EncodeValue (AesonText a) where
  encodeValue = coerce (HI.encodeValue @(HI.AsJsonb a))
```

After:

```haskell
newtype AesonText a = AesonText a
  deriving (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)
  deriving (ToField) via Aeson a
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb a
```

`FromField` stays hand-written. It tries jsonb, then bytea, then text, which is real logic. Its comment says it covers columns that may be TEXT or BYTEA. Under the compatibility rule, those columns should be migrated to `jsonb`, after which this instance becomes `via Aeson a` as well. That is follow-up work: list the tables first with `grep -rn "AesonText" src` plus the column types.

### 4b. `Pkg/DeriveUtils.hs`: base and library types

```haskell
-- before
instance HI.DecodeValue a => HI.DecodeValue (Sum a) where
  decodeValue = Sum <$> HI.decodeValue

instance HI.DecodeValue AET.Value where
  decodeValue = D.jsonb

instance HI.EncodeValue AET.Value where
  encodeValue = E.jsonb

-- after
deriving newtype instance HI.DecodeValue a => HI.DecodeValue (Sum a)
deriving via HI.Jsonb instance HI.DecodeValue AET.Value
deriving via HI.Jsonb instance HI.EncodeValue AET.Value
```

### 4c. `Pkg/DeriveUtils.hs`: `Aeson a` and `Map Text Value` codecs

```haskell
-- before (4 instances, 14 lines)
instance HI.DecodeValue (Map Text AET.Value) where
  decodeValue = D.refine toMap D.jsonb
    where
      toMap (AET.Object o) = Right $ KEM.toMapText o
      toMap other = Left $ "Expected JSON object, got: " <> show other

instance HI.EncodeValue (Map Text AET.Value) where
  encodeValue = contramap (AET.Object . KEM.fromMapText) E.jsonb

instance AET.FromJSON a => HI.DecodeValue (Aeson a) where
  decodeValue = D.refine (bimap toText Aeson . AET.parseEither AET.parseJSON) D.jsonb

instance AET.ToJSON a => HI.EncodeValue (Aeson a) where
  encodeValue = contramap (\(Aeson a) -> AET.toJSON a) E.jsonb

-- after
deriving via HI.AsJsonb (Map Text AET.Value) instance HI.DecodeValue (Map Text AET.Value)
deriving via HI.AsJsonb (Map Text AET.Value) instance HI.EncodeValue (Map Text AET.Value)
deriving via HI.AsJsonb a instance AET.FromJSON a => HI.DecodeValue (Aeson a)
deriving via HI.AsJsonb a instance AET.ToJSON a => HI.EncodeValue (Aeson a)
```

What changes:
- Decode error text changes from `parseEither` on a `Value` to `eitherDecodeStrict` on the bytes.
- Encoding goes through `toEncoding` instead of `toJSON`. Postgres normalises jsonb, so the stored value only differs for a type whose two methods disagree, which would already be a bug in that type.
- For a non-object, the `Map` decode error becomes aeson's message.

No stored data changes.

### 4d. `Models/Projects/GitSync.hs`: `SyncOperation` JSON (18 lines become 1 clause)

The hand-written pair produces exactly aeson's default encoding for a sum mixing constructors with and without fields: `{"tag":"ImportDashboards"}` and `{"tag":"ExportDashboard","contents":"<uuid>"}`. Its stated reason is that "constructor renames cannot change old rows". Under the compatibility rule, a rename comes with a migration that rewrites the `tag`, so derive it:

```haskell
data SyncOperation = ImportDashboards | ExportDashboard DashboardId | ExportDashboards
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via Aeson SyncOperation
```

Keep the round-trip doctests: they now guard the persisted format against an accidental rename. Change the "Unknown" doctest to check for a parse failure (`isLeft`) instead of the old message. Rewrite the haddock as: "Stored as aeson's default tagged encoding; renaming a constructor needs a migration that rewrites `tag`."

### 4e. Smaller derivations

```haskell
-- src/Data/Effectful/CodeReview/Checkout.hs:43  (Envelope: {"artifact": …} / {"result": …})
data Envelope = Artifact TestArtifact | Result FixResult
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.SumObjectWithSingleField, DAE.ConstructorTagModifier '[DAE.CamelToSnake]] Envelope

-- src/Models/Apis/ApiChanges.hs:188
deriving newtype instance Default (Aeson [a])

-- src/Models/Projects/RepositorySync.hs:63 — ResourceKind already derives JSON/DB via WrappedEnumSC
data ResourceKind = Dashboard | Query | Uptime | Cron
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "" ResourceKind
```

`ResourceKind` currently shows the error "Choose dashboard, query, uptime, or cron." The derived message is `Invalid  value: x`, with a double space because the prefix is empty. Fix the shared message once in `shared/src/Pkg/Deriving.hs` rather than keeping a hand-written instance:

```haskell
instance (KnownSymbol prefix, Read a, Show a) => FromHttpApiData (WrappedEnumSC qualType prefix a) where
  parseUrlPiece t = maybeToRight ("Unrecognised value: " <> t) $ WrappedEnumSC <$> decodeEnumSC @prefix (toString t)
```

Listing the valid choices in the message would need `Bounded, Enum` on every one of the ~125 types that derive via `WrappedEnumSC`. Not worth it. `shared/` is loaded prebuilt by the test watcher, so this edit needs a `make tmux-live-test-dev` restart, the one documented exception to never restarting it.

---

## Phase 5 — Single-column `DecodeRow` (`Pkg/DeriveUtils.hs`)

Thirteen instances share one body:

```haskell
instance HI.DecodeRow Text where
  decodeRow = HI.getOneColumn <$> HI.decodeRow
```

`deriving via HI.OneColumn` is impossible: hasql's `Row` has a nominal role, so it cannot be coerced. An `OVERLAPPABLE` catch-all instance would remove all 13, but it weakens instance resolution for the whole project. A record that forgets `deriving anyclass (HI.DecodeRow)` would then fail with a confusing "no `DecodeValue`" error. Use a top-level splice instead, for the 8 concrete types in `DeriveUtils`, placed at the end of the module:

```haskell
-- Types without Generic need DecodeRow via OneColumn.
concat <$> for [[t|UUID.UUID|], [t|Text|], [t|Int64|], [t|Int|], [t|Bool|], [t|UTCTime|], [t|Double|], [t|AET.Value|]] \t ->
  [d|instance HI.DecodeRow $t where decodeRow = HI.getOneColumn <$> HI.decodeRow|]
```

A splice that is only an inline expression does not hit the stage restriction. Keep these by hand:
- the polymorphic ones (`Vector a`, `Maybe a`, and the two `WrappedEnum*` instances, which have contexts);
- the two in other modules (`ProjectMembers:109`, `Git:198`).

An exported helper would save one line at each.

Saves about 20 lines. If the splice reads worse than the 24 lines it replaces when reviewed, drop this phase. It saves the least of the five.

---

## Phase 6 (optional) — `LatencyHist` via `monoidal-containers`

```haskell
-- before
newtype LatencyHist = LatencyHist (IntMap Int64)
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)

instance Semigroup LatencyHist where
  LatencyHist a <> LatencyHist b = LatencyHist $ IntMap.unionWith (+) a b

instance Monoid LatencyHist where
  mempty = LatencyHist IntMap.empty

-- after
newtype LatencyHist = LatencyHist (IntMap Int64)
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving (Semigroup, Monoid) via MonoidalIntMap (Sum Int64)
```

The representation can be coerced: `MonoidalIntMap` is a newtype over `IntMap`, and `Int64` can be coerced to `Sum Int64`. This adds a dependency (`monoidal-containers`, already in Stackage) to `hpack-includes/lib-deps.yaml`, as AGENTS.md prefers. It is one site, so do it only if the dependency is wanted for other reasons.

---

## Not doing

| Instance | Why it stays |
|---|---|
| `GitHost` DB/HTTP codecs (`Pkg/Git.hs:182–206`) | The slug is part of webhook URLs registered at customers' Git hosts (`webhook/git/<slug>`) and of the `git_sync.host` CHECK constraint. `WrappedEnumSC` would produce `git_hub`. We can migrate the DB, but we cannot migrate URLs registered outside it. |
| `DeploymentEnv` `Default` | Deriving it would make constructor order carry meaning (the first one is the default). Two explicit lines are clearer. |
| `UsageCost`, `ResourceUsage`, `HopAgg` `Semigroup` | Deriving needs every field wrapped in `Sum`, which adds `getSum` at every reader. Not worth it for 3 sites. |
| `FromHttpApiData` for `[a]`, `Vector a`, `Value` (`Utils.hs:858–866`) | `via JSONHttpApiData` adds a retry that parses the value as a quoted string. For `Value`, `foo` would then silently become `String "foo"` instead of an error. |
| ~10 `ToHtml` instances that exist only for `outcomeBody = toHtml` | Folding them into `outcomeBody` is not a derivation, and it would weaken the typed-`ToHtml` convention. |
| `HasField` on `SlackChannel` | A doctest asserts that record update is impossible. It is a smart constructor. |
| Everything else (≈ 340 instances) | Real logic: validation behind smart constructors, wire formats that no derived encoding produces, lenient parsing, computed fields, per-constructor messages, redacting `Show`, rendering. |

## Verification

1. After each phase: read the text after the last `Reloading` in `build.log` and check it for `error`. Do not run `cabal build`.
2. Phases 1–2: `.test-args` focused on `DashboardWidgetsSpec`. Both new regression tests fail before the fix and pass after it.
3. Phase 3: focus on `/Pages/` to cover the converted handlers' toast and refusal assertions.
4. Phase 4d: `make test-doctests` for the `SyncOperation` doctests.
5. Phase 4e (`shared/` edit): restart the test watcher once, then run the full suite.
6. Then run `/hs-distill`, `/hs-evasion-review` and `/hs-lob-review` until a pass finds nothing material, then `/simplify`, `make fmt lint`, and `make ci-signoff` once at the end.
