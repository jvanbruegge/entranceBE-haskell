FROM fpco/stack-build-small:lts-13.24 as build

RUN mkdir /opt/build
WORKDIR /opt/build

# Cache dependencies
COPY stack.yaml .
COPY package.yaml .
RUN stack build --only-dependencies --system-ghc

COPY . .
RUN stack install --system-ghc

FROM ubuntu:16.04

RUN apt-get update && apt-get install -y libgmp-dev ca-certificates netbase

RUN mkdir -p /opt/entranceBE
WORKDIR /opt/entranceBE
COPY --from=build /root/.local/bin/entranceBE .

EXPOSE 8000

CMD ["/opt/entranceBE/entranceBE"]
