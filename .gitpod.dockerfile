FROM gitpod/workspace-full:latest
USER gitpod
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh 
RUN stack install ormolu
RUN ghcup install hls
ENV PATH=/home/gitpod/.local/bin:$PATH