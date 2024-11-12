FROM ocaml/opam:debian-ocaml-5.2
SHELL ["/bin/bash", "-c"]
RUN sudo apt-get update && \
    sudo apt install -y build-essential libgmp-dev z3 pkg-config zlib1g-dev python3 python3-pip python3.11-venv
RUN opam update && opam install sail.0.18 ounit2
RUN opam repo add coq-released https://coq.inria.fr/opam/released
RUN opam pin add coq 8.18.0
RUN opam pin add coq-iris 4.1.0
RUN opam install coq-equations
RUN opam repo add katamaran https://github.com/katamaran-project/opam-repository.git
RUN opam install coq-katamaran
RUN git clone https://github.com/katamaran-project/sail-backend.git
RUN cd sail-backend && \
    python3 -m venv .venv && \
    ./.venv/bin/pip install psutil
RUN cd sail-backend && \
    opam install . --deps-only -y
RUN source .profile && \
    cd sail-backend && \
    source ./.venv/bin/activate && make build