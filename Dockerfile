FROM ocaml/opam:debian-ocaml-5.1
RUN sudo apt-get update && sudo apt install -y build-essential libgmp-dev z3 pkg-config zlib1g-dev
RUN opam install sail ounit2
RUN git clone https://github.com/katamaran-project/sail-backend.git && \
    cd sail-backend # && \
    # opam install . --working-dir .

