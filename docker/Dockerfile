FROM ocaml/opam:debian-ocaml-5.2
SHELL ["/bin/bash", "-c"]
RUN sudo apt-get update && \
    sudo apt install -y build-essential libgmp-dev z3 pkg-config zlib1g-dev python3 python3-pip python3.11-venv
RUN opam update && opam install sail.0.18 ounit2
RUN git clone https://github.com/katamaran-project/sail-backend.git && \
    cd sail-backend && \
    opam install --deps-only . && \
    python3 -m venv .venv && \
    ./.venv/bin/pip install psutil
RUN source .profile && cd sail-backend && source ./.venv/bin/activate && make build