FROM haskell:8.6 AS builder
WORKDIR /opt/laser-egg
ADD . .
RUN stack --install-ghc install && strip /root/.local/bin/laser-egg

FROM debian:stretch
## ensure locale is set during build
ENV LANG            C.UTF-8
RUN apt-get update && \
    apt-get install -y --no-install-recommends libgmp10
COPY --from=builder /root/.local/bin/laser-egg /usr/bin/laser-egg
