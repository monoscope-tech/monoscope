FROM ubuntu:24.04
RUN mkdir -p /opt/apitoolkit/
# Set the LANG environment variable
ENV LANG C.UTF-8
WORKDIR /opt/apitoolkit
RUN apt-get update && apt-get install -y \
  ca-certificates \
  netbase \
  curl \
  libgmp-dev \
  libgrpc-dev \
  librdkafka-dev \
  libpq-dev
COPY apitoolkit-server-exe /opt/apitoolkit
COPY static /opt/apitoolkit/static
CMD ["/opt/apitoolkit/apitoolkit-server-exe"]
