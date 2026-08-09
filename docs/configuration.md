# Configuration Guide

This guide covers all configuration options for Monoscope.

## Configuration Methods

Monoscope can be configured through:
1. Environment variables (recommended)
2. `.env` file
3. Docker Compose environment section

## Core Configuration

### Server Settings

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `PORT` | HTTP server port | 8080 | No |
| `GRPC_PORT` | gRPC server port for OpenTelemetry | 4317 | No |
| `ENVIRONMENT` | Environment (DEV/STAGING/PROD) | DEV | No |
| `HOST_URL` | Public URL of your Monoscope instance | http://localhost:8080 | No |

### Database Configuration

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `DATABASE_URL` | PostgreSQL connection string | - | Yes |
| `MIGRATE_AND_INITIALIZE_ON_START` | Run migrations on startup | True | No |
| `MIGRATIONS_DIR` | Path to migration files | /app/static/migrations/ | No |

Example DATABASE_URL:
```
postgresql://user:password@localhost:5432/monoscope?sslmode=disable
```

### Authentication

#### Basic Authentication
| Variable | Description | Default |
|----------|-------------|---------|
| `BASIC_AUTH_ENABLED` | Enable basic auth | True |
| `BASIC_AUTH_USERNAME` | Username | admin |
| `BASIC_AUTH_PASSWORD` | Password | changeme |

#### Auth0 Configuration
| Variable | Description | Required |
|----------|-------------|----------|
| `AUTH0_DOMAIN` | Auth0 domain | Yes (if using Auth0) |
| `AUTH0_CLIENT_ID` | Auth0 client ID | Yes (if using Auth0) |
| `AUTH0_SECRET` | Auth0 client secret | Yes (if using Auth0) |
| `AUTH0_CALLBACK` | Callback URL | Yes (if using Auth0) |
| `AUTH0_LOGOUT_REDIRECT` | Logout redirect URL | Yes (if using Auth0) |

#### Generic OpenID Connect

Generic OIDC controls only interactive browser authentication at `/login`,
`/auth_callback`, and `/logout`. API keys, OTLP ingestion, CLI authentication,
and other machine routes are unchanged. Basic Auth is a separate mode and takes
precedence for browser requests, so set `BASIC_AUTH_ENABLED=False` when OIDC
should drive browser login.

When `OIDC_ENABLED=True`, startup fails closed if required configuration is
missing, discovery metadata is invalid, the issuer differs, or production
endpoints are not HTTPS. Register `OIDC_CALLBACK_URL` and
`OIDC_LOGOUT_REDIRECT` exactly with the provider. Monoscope uses Authorization
Code with PKCE S256 and `client_secret_basic`.

| Variable | Description | Default |
|---|---|---|
| `OIDC_ENABLED` | Enable generic OIDC for interactive routes | `False` |
| `OIDC_ISSUER` | Exact issuer identifier expected in discovery and ID tokens | Required when enabled |
| `OIDC_DISCOVERY_URL` | OpenID Provider configuration URL | `<issuer>/.well-known/openid-configuration` |
| `OIDC_CLIENT_ID` | Registered client ID | Required when enabled |
| `OIDC_CLIENT_SECRET` | Registered client secret | Required when enabled |
| `OIDC_CALLBACK_URL` | Fixed registered callback URL | Required when enabled |
| `OIDC_LOGOUT_REDIRECT` | Registered post-logout redirect URL | Required when enabled |
| `OIDC_SCOPES` | Space- or comma-separated scopes; must contain `openid` | `openid profile email` |
| `OIDC_ALLOWED_ALGORITHMS` | Explicit asymmetric ID-token algorithms | `RS256` |
| `OIDC_EXISTING_USER_LINK_MODE` | `disabled` or `verified_email` linking | `disabled` |
| `OIDC_AUTO_REGISTER` | Create active regular users without memberships | `False` |

`verified_email` links an unlinked identity to exactly one existing local user
only when the provider explicitly reports `email_verified=true`. OIDC claims
never grant sudo, project, team, or administrator access. Auto-registered users
require an email claim and start without project or team memberships. A safe
migration from Basic Auth is
to set `verified_email` for the first login of an existing user whose local email
already matches the provider's verified email, confirm that the identity is
linked, then return the mode to `disabled`. Keep auto-registration disabled
unless new regular, unprivileged accounts are intentional.

```env
BASIC_AUTH_ENABLED=False
OIDC_ENABLED=True
OIDC_ISSUER=https://identity.example.com
OIDC_DISCOVERY_URL=https://identity.example.com/.well-known/openid-configuration
OIDC_CLIENT_ID=monoscope
OIDC_CLIENT_SECRET=replace-with-registered-client-secret
OIDC_CALLBACK_URL=https://monoscope.example.com/auth_callback
OIDC_LOGOUT_REDIRECT=https://monoscope.example.com/
OIDC_SCOPES=openid profile email
OIDC_ALLOWED_ALGORITHMS=RS256
OIDC_EXISTING_USER_LINK_MODE=verified_email
OIDC_AUTO_REGISTER=False
```

### Storage & Data Processing

| Variable | Description | Default |
|----------|-------------|---------|
| `API_KEY_ENCRYPTION_SECRET_KEY` | Secret for API key encryption (min 32 chars) | - |
| `MESSAGES_PER_PUBSUB_PULL_BATCH` | Batch size for message processing | 100 |
| `ENABLE_BACKGROUND_JOBS` | Enable background job processing | True |
| `MAX_CONCURRENT_JOBS` | Maximum concurrent background jobs | 4 |
| `ENABLE_DAILY_JOB_SCHEDULING` | Enable daily scheduled jobs | True |

### OpenTelemetry & Ingestion

| Variable | Description | Default |
|----------|-------------|---------|
| `TELEMETRY_PROJECT_ID` | Project ID for telemetry data | - |
| `TELEMETRY_SERVICE_NAME` | Service name for telemetry | monoscope |
| `ENABLE_EVENTS_TABLE_UPDATES` | Enable events table updates | True |

### Notification Services

#### Email Configuration (SendGrid)
| Variable | Description |
|----------|-------------|
| `SENDGRIDAPIKEY` | SendGrid API key |

#### Email Configuration (SMTP)
| Variable | Description |
|----------|-------------|
| `SMTP_HOST` | SMTP server hostname |
| `SMTP_PORT` | SMTP server port |
| `SMTP_USERNAME` | SMTP username |
| `SMTP_PASSWORD` | SMTP password |
| `SMTP_SENDER` | From email address |

#### Slack Integration
| Variable | Description |
|----------|-------------|
| `SLACK_CLIENT_ID` | Slack app client ID |
| `SLACK_CLIENT_SECRET` | Slack app client secret |
| `SLACK_REDIRECT_URI` | Slack OAuth redirect URI |
| `SLACK_BOT_TOKEN` | Slack bot token |

#### Discord Integration
| Variable | Description |
|----------|-------------|
| `DISCORD_CLIENT_ID` | Discord app client ID |
| `DISCORD_CLIENT_SECRET` | Discord app client secret |
| `DISCORD_REDIRECT_URI` | Discord OAuth redirect URI |
| `DISCORD_BOT_TOKEN` | Discord bot token |
| `DISCORD_WEBHOOK_URL` | Discord webhook URL |

### AI & LLM Configuration

| Variable | Description | Default |
|----------|-------------|---------|
| `OPENAI_API_KEY` | OpenAI API key for AI features | - |
| `OPENAI_BASE_URL` | Custom OpenAI API endpoint | https://api.openai.com |

### Feature Flags

| Variable | Description | Default |
|----------|-------------|---------|
| `SHOW_DEMO_PROJECT` | Show demo project in UI | False |
| `ENABLE_BROWSER_MONITORING` | Enable browser monitoring | True |
| `ENABLE_FREETIER` | Enable free tier limitations | False |
| `ENABLE_REPLAY_SERVICE` | Enable session replay | False |

### Logging

| Variable | Description | Default | Options |
|----------|-------------|---------|---------|
| `LOGGING_DESTINATION` | Where to send logs | StdOut | StdOut, GCP |
| `LOG_LEVEL` | Logging level | info | trace, info, attention |

## Example Configurations

### Minimal Development Setup

```env
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/monoscope
BASIC_AUTH_ENABLED=True
BASIC_AUTH_USERNAME=admin
BASIC_AUTH_PASSWORD=changeme
API_KEY_ENCRYPTION_SECRET_KEY=monoscope123456monoscope1234567
```

### Production with Auth0

```env
DATABASE_URL=postgresql://prod_user:secure_pass@db.example.com:5432/monoscope?sslmode=require
ENVIRONMENT=PROD
HOST_URL=https://monoscope.example.com

# Disable basic auth
BASIC_AUTH_ENABLED=False

# Auth0 configuration
AUTH0_DOMAIN=example.auth0.com
AUTH0_CLIENT_ID=your-client-id
AUTH0_SECRET=your-secret
AUTH0_CALLBACK=https://monoscope.example.com/auth_callback
AUTH0_LOGOUT_REDIRECT=https://monoscope.example.com

# Security
API_KEY_ENCRYPTION_SECRET_KEY=your-32-character-or-longer-secret-key

# Email notifications
SENDGRIDAPIKEY=your-sendgrid-key
```

### High-Performance Configuration

```env
# Increase batch processing
MESSAGES_PER_PUBSUB_PULL_BATCH=500
MAX_CONCURRENT_JOBS=16

# Optimize database connections
DATABASE_URL=postgresql://user:pass@localhost:5432/monoscope?pool_size=50

# Enable all performance features
ENABLE_BACKGROUND_JOBS=True
ENABLE_EVENTS_TABLE_UPDATES=True
```

## Configuration Best Practices

1. **Use strong secrets**: Generate random strings for API_KEY_ENCRYPTION_SECRET_KEY
   ```bash
   openssl rand -hex 32
   ```

2. **Secure database connections**: Always use SSL in production
   ```
   DATABASE_URL=postgresql://...?sslmode=require
   ```

3. **Use .env files**: Keep secrets out of docker-compose.yml
   ```bash
   # .env file
   SENSITIVE_KEY=secret_value

   # docker-compose.yml
   environment:
     - SENSITIVE_KEY=${SENSITIVE_KEY}
   ```

4. **Set appropriate resource limits**: Configure MAX_CONCURRENT_JOBS based on available CPU cores

5. **Enable only needed integrations**: Don't configure unused notification services

## Troubleshooting Configuration

### Verify configuration is loaded
```bash
docker-compose exec monoscope env | grep MONOSCOPE
```

### Check for configuration errors
```bash
docker-compose logs monoscope | grep -i error
```

### Test database connection
```bash
docker-compose exec monoscope psql $DATABASE_URL -c "SELECT 1"
```
