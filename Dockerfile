FROM ubuntu:22.04
RUN mkdir -p /opt/apitoolkit/
ARG BINARY_PATH
WORKDIR /opt/apitoolkit
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY apitoolkit-server-exe /opt/apitoolkit
COPY static /opt/apitoolkit
CMD ["/opt/apitoolkit/apitoolkit-server-exe"]
