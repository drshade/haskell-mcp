# Build stage
FROM haskell:9.10 AS builder

RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get update --allow-releaseinfo-change && \
    apt-get install -y wget ca-certificates && \
    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    echo "deb http://apt.postgresql.org/pub/repos/apt/ bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
    apt-get update && \
    apt-get install -y \
    postgresql-server-dev-15 \
    libpq-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update

# Copy and build
WORKDIR /app
COPY haskell-mcp.cabal /app
COPY CHANGELOG.md /app
COPY LICENSE /app

RUN cabal build all --only-dependencies

COPY app /app/app
RUN cabal build
RUN cabal install --installdir=/usr/local/bin

# Runtime stage
FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y \
    libpq5 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/bin/haskell-mcp /usr/local/bin/haskell-mcp

WORKDIR /app

# Let's not bake these into the image
# COPY .credential-salesforce /app
# COPY .credential-ruddr /app

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/haskell-mcp"]