FROM gitpod/workspace-full:latest
USER gitpod
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh 
RUN stack install brittany hlint
RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules \
    && cd haskell-ide-engine  \
    && stack install haskell-ide-engine \
    && cd .. \
    && rm -rf haskell-ide-engine \
    && cd apitoolkit-server \
    && stack run
ENV PATH=/home/gitpod/.local/bin:$PATH