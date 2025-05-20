FROM haskell:9.12

RUN cabal update

# Copy and build
WORKDIR /app
COPY sfdc-mcp.cabal /app
COPY CHANGELOG.md /app
COPY LICENSE /app

RUN cabal build all --only-dependencies

COPY app /app/app
RUN cabal build
RUN cabal install --installdir=/usr/local/bin

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/sfdc-mcp"]