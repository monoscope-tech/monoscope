# Single build stage using pre-built deps image
# Contains: GHC, Node.js, bun, all npm/cabal deps pre-installed, chart-cli pre-built
FROM ghcr.io/monoscope-tech/monoscope-deps:latest AS builder

# Install system dependencies if not using deps image (no-ops if already installed)
RUN apt-get update && apt-get install -y --no-install-recommends \
  curl ca-certificates lsb-release gnupg \
  libssl-dev librdkafka-dev libsnappy-dev libgrpc-dev \
  libpq-dev libldap2-dev libsasl2-dev liblz4-dev libzstd-dev \
  pkg-config git wget unzip \
  || true

# Install protoc if not present
RUN which protoc || ( \
  PROTOC_VERSION=29.3 && \
  ARCH=$(dpkg --print-architecture | sed 's/arm64/aarch_64/;s/amd64/x86_64/') && \
  wget -q https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-${ARCH}.zip && \
  unzip -q protoc-${PROTOC_VERSION}-linux-${ARCH}.zip -d /usr/local && \
  rm protoc-${PROTOC_VERSION}-linux-${ARCH}.zip \
  )

# Install postgresql-client if not present
RUN which psql || ( \
  curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/postgresql-keyring.gpg && \
  echo "deb [signed-by=/usr/share/keyrings/postgresql-keyring.gpg] http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
  apt-get update && apt-get install -y postgresql-client-16 \
  )

RUN rm -rf /var/lib/apt/lists/* || true

WORKDIR /build

# Copy git metadata for GitHash
COPY .git .git

# Copy cabal files for dependency caching
COPY *.cabal cabal.project* Setup.hs LICENSE README.md auto-instrument-config.toml ./

# Build Haskell dependencies (fast - already cached in deps image)
RUN cabal update && cabal build --only-dependencies exe:monoscope -j --semaphore

# Copy source code
COPY src ./src
COPY test ./test
COPY app ./app
COPY proto ./proto

# Build frontend assets (npm deps already installed in deps image)
COPY config ./config
COPY static ./static
COPY web-components ./web-components
RUN npx tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --minify && \
  cd web-components && NODE_ENV=production npx vite build --mode production --sourcemap false && \
  cd .. && workbox generateSW config/workbox-config.js

# Build Haskell executable (dist-newstyle cached via Docker layer cache)
RUN cabal build exe:monoscope -j --semaphore --ghc-options="+RTS -A64m -n2m -RTS" && \
  mkdir -p /build/dist && \
  find dist-newstyle -name monoscope -type f -executable | head -1 | xargs -I {} cp {} /build/dist/

# Final runtime image
FROM debian:12-slim

# Install runtime dependencies
# Graphics libs needed for @napi-rs/canvas in chart-cli
RUN apt-get update && apt-get install -y --no-install-recommends \
  ca-certificates \
  libgmp10 \
  librdkafka1 \
  libpq5 \
  libsnappy1v5 \
  liblz4-1 \
  libzstd1 \
  libsasl2-2 \
  libldap-2.5-0 \
  libcairo2 \
  libpango-1.0-0 \
  libpangocairo-1.0-0 \
  libjpeg62-turbo \
  libgif7 \
  fontconfig \
  fonts-dejavu-core \
  && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -U -s /bin/false monoscope

WORKDIR /opt/monoscope

# Copy artifacts
COPY --from=builder /build/dist/monoscope ./
COPY --from=builder /build/static ./static
COPY --from=builder /usr/local/bin/chart-cli ./

# Set ownership and permissions
RUN chown -R monoscope:monoscope /opt/monoscope && \
  chmod +x monoscope chart-cli

USER monoscope

EXPOSE 8080
ENTRYPOINT ["./monoscope"]
