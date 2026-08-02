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

# Copy cabal files for dependency caching
COPY *.cabal cabal.project* Setup.hs LICENSE README.md auto-instrument-config.toml ./

# Production profiling (Haskell counterpart of timefusion's --features profiling):
# prof-way build with LATE cost centres on monoscope code only — deps are built
# prof-way but with no cost centres, so runtime overhead (~2-5%) stays on our
# code. Enables +RTS -p CPU samples and -hc heap censuses (see GHCRTS below).
RUN printf 'package *\n  profiling: True\n  profiling-detail: none\npackage monoscope\n  profiling-detail: late\n' >> cabal.project.local

# Build Haskell dependencies (fast - already cached in deps image)
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
    cabal update && cabal build --only-dependencies exe:monoscope-server -j --semaphore

# Copy source code
COPY package.yaml ./
COPY src ./src
COPY test ./test
COPY app ./app
COPY cli ./cli
COPY proto ./proto

# Build frontend assets (npm deps already installed in deps image)
COPY config ./config
COPY static ./static
COPY web-components ./web-components
RUN npx tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --minify && \
  cd web-components && NODE_ENV=production npx vite build --mode production --sourcemap false && \
  cd .. && workbox generateSW config/workbox-config.js

# Build Haskell executable (dist-newstyle persisted via BuildKit cache mount)
RUN --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/build/dist-newstyle \
    (command -v hpack >/dev/null && hpack || echo "hpack not installed, using committed monoscope.cabal") && \
    cabal build exe:monoscope-server -j --semaphore --ghc-options="+RTS -A64m -n2m -RTS" && \
    mkdir -p /build/dist && \
    find dist-newstyle -name monoscope-server -type f -executable | head -1 | xargs -I {} cp {} /build/dist/

# Final runtime image
FROM debian:12-slim

ARG GIT_HASH=dev
ARG GIT_COMMIT_DATE=dev
ENV GIT_HASH=$GIT_HASH
ENV GIT_COMMIT_DATE=$GIT_COMMIT_DATE

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
COPY --from=builder /build/dist/monoscope-server ./
COPY --from=builder /build/static ./static
COPY --from=builder /usr/local/bin/chart-cli ./

# Set ownership and permissions
RUN mkdir -p profiles && \
  chown -R monoscope:monoscope /opt/monoscope && \
  chmod +x monoscope-server chart-cli

USER monoscope

EXPOSE 8080

# Production profiling (binary is built prof-way with late cost centres):
# heap census every 60s (-hc -i60) and CPU cost-centre samples (-p, duty-cycled
# 60s/15min by the cpu-profiler fiber in System.Server) stream to the eventlog,
# flushed every 30s so the samples leading up to an OOM kill survive. -l-a
# disables all other event classes to keep the file small.
# Starts DISABLED: enable after deployment with `docker kill -s USR2 <ctr>`
# (same signal toggles it back off, no restart). Mount ./profiles as a volume
# to keep artifacts across redeploys. Analyze off-host: eventlog2html (heap),
# hs-speedscope (CPU); a .prof is written on clean exit.
ENV GHCRTS="-p -poprofiles/monoscope -l-a --eventlog-flush-interval=30 -hc -i60"

# Liveness of the web listener itself (not just the process): a wedged warp
# accept-loop leaves the container "running" but refusing connections, so Swarm
# keeps routing 1/N of VIP traffic to a black-hole. Probe /status (bare 200, no
# auth redirect) over bash /dev/tcp — the image has no curl/wget. ${PORT:-8080}
# matches prod (PORT=80) and the exposed default.
HEALTHCHECK --interval=15s --timeout=5s --start-period=90s --retries=3 \
  CMD ["bash","-c","exec 3<>/dev/tcp/127.0.0.1/${PORT:-8080}; printf 'GET /status HTTP/1.0\\r\\nHost: localhost\\r\\n\\r\\n' >&3; head -1 <&3 | grep -q '200 OK'"]

# Timestamped eventlog per boot: a fixed -ol path would be truncated by the
# restart right after an OOM kill, destroying exactly the samples we want.
# Prune to the newest 5 so a crash-loop can't fill the volume (cf. timefusion's
# prune_old). Command-line RTS opts take precedence over GHCRTS.
ENTRYPOINT ["bash","-c","ls -t profiles/monoscope-*.eventlog 2>/dev/null | tail -n +6 | xargs -r rm -f; exec ./monoscope-server +RTS -olprofiles/monoscope-$(date +%s).eventlog -RTS \"$@\"","--"]
