FROM ubuntu:22.04
RUN mkdir -p /opt/apitoolkit/
WORKDIR /opt/apitoolkit
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  libpq-dev
COPY apitoolkit-server-exe /opt/apitoolkit
COPY static /opt/apitoolkit/static
CMD ["/opt/apitoolkit/apitoolkit-server-exe"]
