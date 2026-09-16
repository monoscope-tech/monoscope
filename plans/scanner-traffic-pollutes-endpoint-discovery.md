# Scanner traffic is discovered as API endpoints, and notifies

## Finding

Inbound requests to a **bare-IP host** are internet background-noise scans, and endpoint
discovery treats each one as a newly found API endpoint — which raises an `api_change`
issue, which is the notification gate.

Measured 2026-09-16 against prod:

| | |
|---|---|
| endpoints repo-wide whose `host` is a bare IPv4 | **7,665 of 28,440 (27%)**, across 20 projects |
| of those, inbound (`outgoing = false`) | **7,630** — only 35 are real outbound calls to IP-addressed services |
| `api_change` issues in the last 7 days for an IP-hosted endpoint | **3,896 of 7,206 (54%)** |

Worst single project, "Engine/API Prod" (`d062e010…`), last 8 hours: 121 issues, 120 of
them `api_change`. 110 of the 120 new endpoints came from one raw IP (`34.54.219.80`),
6 from another; only 4 came from their actual domains.

The paths are unambiguous:

```
/api/.env/wp-admin/leafmailer     /.ssh/id_dsa.priv        /.azure/credentials
/service_account_key.json         /.env-release.log        /core/Database/.env
/home/ec2-user/.local/bin/aws/config/.env
/dns-query  /resolve  /query      (DNS-over-HTTPS probes on raw IPs)
```

## Why the existing guard misses it

`ProcessMessage.processSpanToEntities` already refuses to create an endpoint for a 404:

```haskell
!isNewEndpoint = notElem endpointHash pjc.endpointHashes && statusCode /= 404
```

These scans do not 404 — a load balancer answers them with 200/301/403 before the app
sees them, so the guard never fires. See also [[endpoint_evidence_and_test_recipe]]:
spread, not status, is what proves a route.

## Two options, in increasing boldness

1. **Stop notifying, keep the data.** Suppress only the `api_change` issue when the
   endpoint's host is a bare IP and the span is inbound. The catalog still records what
   was hit (useful for a security view), but 54% of endpoint notifications stop.
2. **Stop discovering.** Extend `isNewEndpoint` so an inbound bare-IP host never creates
   an endpoint at all. Also removes the 27% catalog pollution, but is lossy.

Either way the condition must be `bare-IP host AND NOT outgoing` — the 35 outgoing
IP-addressed endpoints are legitimate calls to internal services and must survive.

## The risk that makes this a judgement call

A customer who genuinely serves an API addressed by IP (internal tooling, no DNS) would
lose their catalog under option 2, and their change notifications under option 1. Nothing
in the data distinguishes them from a scanner except the paths, and path-matching a
blocklist is a losing game.

A middle road worth considering: keep discovering, but only notify once a bare-IP endpoint
shows **repeat traffic from more than one client** — a scanner hits a path once, a real
endpoint gets used.

## Verification recipe

```sql
-- the population
select count(*) filter (where host ~ '^[0-9]{1,3}(\.[0-9]{1,3}){3}(:[0-9]+)?$') as ip_hosted,
       count(*) as total
from apis.endpoints;

-- inbound vs outgoing split
select outgoing, count(*) from apis.endpoints
where host ~ '^[0-9]{1,3}(\.[0-9]{1,3}){3}(:[0-9]+)?$' group by 1;

-- share of notifications
select count(*) filter (where e.host ~ '^[0-9]{1,3}(\.[0-9]{1,3}){3}(:[0-9]+)?$') as from_ip,
       count(*) as total_api_change
from apis.issues i join apis.endpoints e on e.project_id = i.project_id and e.hash = i.endpoint_hash
where i.issue_type = 'api_change' and i.created_at > now() - interval '7 days';
```
