BEGIN;

CREATE TABLE users.oidc_pending_auth_requests
(
  state_hash    BYTEA        PRIMARY KEY CHECK (octet_length(state_hash) = 32),
  nonce         TEXT         NOT NULL CHECK (length(nonce) BETWEEN 43 AND 128),
  code_verifier TEXT         NOT NULL CHECK (length(code_verifier) BETWEEN 43 AND 128),
  redirect_to   TEXT         NOT NULL CHECK (redirect_to ~ '^/($|[^/])'),
  expires_at    TIMESTAMPTZ  NOT NULL
);

CREATE INDEX oidc_pending_auth_requests_expiry_idx
  ON users.oidc_pending_auth_requests (expires_at);

CREATE TABLE users.oidc_identities
(
  issuer     TEXT NOT NULL,
  subject    TEXT NOT NULL,
  user_id    UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  PRIMARY KEY (issuer, subject),
  CONSTRAINT oidc_identities_one_per_issuer_per_user UNIQUE (issuer, user_id),
  CHECK (length(issuer) BETWEEN 1 AND 2048),
  CHECK (length(subject) BETWEEN 1 AND 255)
);

COMMIT;
