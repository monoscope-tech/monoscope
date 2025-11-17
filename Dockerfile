# Stage 1: Build frontend assets
FROM node:18-alpine AS frontend-builder

WORKDIR /build

# Copy package files
COPY package*.json ./
COPY web-components/package*.json ./web-components/

# Install dependencies
RUN --mount=type=cache,target=/root/.npm \
  npm ci --prefer-offline --no-audit && \
  cd web-components && npm ci --prefer-offline --no-audit

# Copy source files
COPY tailwind.config.js ./
COPY static ./static
COPY web-components ./web-components
# Copy Haskell source files for Tailwind CSS scanning
COPY src ./src

# Build assets
RUN npx tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --minify && \
  cd web-components && NODE_ENV=production npx vite build --mode production --sourcemap false

# Copy workbox config
COPY workbox-config.js ./

# Generate service worker after building assets
RUN npx workbox generateSW workbox-config.js

# Stage 2: Build Haskell application
FROM haskell:9.12.2 AS haskell-builder

# Install system dependencies (combined into single layer)
RUN apt-get update && apt-get install -y \
  curl ca-certificates lsb-release gnupg \
  libssl-dev librdkafka-dev libsnappy-dev libgrpc-dev \
  libpq-dev libldap2-dev libsasl2-dev liblz4-dev libzstd-dev \
  pkg-config git wget unzip \
  && curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/postgresql-keyring.gpg \
  && echo "deb [signed-by=/usr/share/keyrings/postgresql-keyring.gpg] http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
  && apt-get update && apt-get install -y postgresql-client-16 \
  && rm -rf /var/lib/apt/lists/*

# Install protoc (simplified)
RUN PROTOC_VERSION=29.3 && \
  ARCH=$(dpkg --print-architecture | sed 's/arm64/aarch_64/;s/amd64/x86_64/') && \
  wget -q https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-${ARCH}.zip && \
  unzip -q protoc-${PROTOC_VERSION}-linux-${ARCH}.zip -d /usr/local && \
  rm protoc-${PROTOC_VERSION}-linux-${ARCH}.zip

WORKDIR /build

# Copy git metadata for GitHash (actual git info from the repo)
COPY .git .git

# Copy cabal files for dependency caching
COPY *.cabal cabal.project* Setup.hs LICENSE README.md auto-instrument-config.toml ./

# Build dependencies with parallel jobs
RUN --mount=type=cache,target=/root/.cabal/store \
  --mount=type=cache,target=/build/dist-newstyle \
  cabal update && \
  cabal build --only-dependencies exe:monoscope -j$(nproc)

# Copy source code
COPY src ./src
COPY test ./test
COPY app ./app
COPY proto ./proto

# Copy built frontend assets from frontend-builder stage
COPY --from=frontend-builder /build/static ./static

# Build executable
RUN --mount=type=cache,target=/root/.cabal/store \
  --mount=type=cache,target=/build/dist-newstyle \
  cabal build exe:monoscope -j$(nproc) && \
  mkdir -p /build/dist && \
  find dist-newstyle -name monoscope -type f -executable | head -1 | xargs -I {} cp {} /build/dist/

# Stage 3: Final runtime image using debian slim
FROM debian:12-slim

# Install only essential runtime dependencies
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
  && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -U -s /bin/false monoscope

WORKDIR /opt/monoscope

# Copy artifacts
COPY --from=haskell-builder /build/dist/monoscope ./
COPY --from=frontend-builder /build/static ./static

# Set ownership and permissions
RUN chown -R monoscope:monoscope /opt/monoscope && \
  chmod +x monoscope

USER monoscope

EXPOSE 8080
ENTRYPOINT ["./monoscope"]
