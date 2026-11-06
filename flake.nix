{
  nixConfig = {
    extra-substituters = [ "https://katamaran.cachix.org" ];
    extra-trusted-public-keys = [ "katamaran.cachix.org-1:6oOkbLd/LRTGiixl0cVyWC2/OXo5A0l1e84jqZf+dCE=" ];
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    katamaran = {
      # Katamaran main, built with Rocq 9.2 (its packages.rocq920).
      url = "github:katamaran-project/katamaran/4f7cef1e8e10d5a0d244c64ffd8e26c9137ea40a";
      inputs.flake-utils.follows = "flake-utils";
    };
    # Use Katamaran's nixpkgs, so that Rocq and its libraries are the same
    # derivations as Katamaran's (and can come from its cachix).
    nixpkgs.follows = "katamaran/nixpkgs";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };

      # Keep in sync with OCAML_VERSION and SAIL_VERSION in .github/workflows/build.yaml.
      sailVersion = "0.20.3";
      ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2.overrideScope (self: super: {
        sail = super.sail.overrideAttrs (old: {
          version = sailVersion;
          src = pkgs.fetchurl {
            url = "https://github.com/rems-project/sail/releases/download/${sailVersion}/sail-${sailVersion}.tbz";
            hash = "sha256-CyI+2D9SGth+qs2IGGOQ+68LlE9jxqBKPr35ah/1pgw=";
          };
          # nixpkgs' menhir compatibility patch is already part of 0.20.3.
          patches = [ ];
        });
      });

      katamaran = inputs.katamaran.packages.${system}.rocq920;

      # Rocq (with the coqc compatibility binaries) that Katamaran was built with.
      # Katamaran's flake does not export it; it is the only buildInput of Katamaran.
      rocq = pkgs.lib.findFirst (d: pkgs.lib.hasPrefix "coq-" d.name)
        (throw "flake.nix: no Rocq found in Katamaran's buildInputs")
        katamaran.buildInputs;

      # Rocq is built with a different OCaml than this project, so Rocq and Katamaran cannot
      # be ordinary shell inputs: their findlib and OCaml libraries would conflict with ours.
      # Instead, wrap Rocq's binaries so that only they see those libraries (OCAMLPATH, for
      # plugins such as Equations) and Katamaran with its Rocq dependencies (ROCQPATH).
      rocqWrapped = pkgs.runCommand "rocq-katamaran" {
        nativeBuildInputs = [ pkgs.makeWrapper ];
        deps = [ rocq katamaran ] ++ rocq.propagatedBuildInputs ++ katamaran.propagatedBuildInputs;
      } ''
        ocamlpath=; rocqpath=
        for d in $deps; do
          for p in $d/lib/ocaml/*/site-lib; do [ -d "$p" ] && ocamlpath="$ocamlpath''${ocamlpath:+:}$p"; done
          for p in $d/lib/coq/*/user-contrib; do [ -d "$p" ] && rocqpath="$rocqpath''${rocqpath:+:}$p"; done
        done
        mkdir -p $out/bin
        for b in ${rocq}/bin/*; do
          makeWrapper "$b" "$out/bin/$(basename "$b")" \
            --set OCAMLPATH "$ocamlpath" --set ROCQPATH "$rocqpath" --unset COQPATH
        done
      '';
    in
    rec {
      packages = rec {
        monads = ocamlPackages.callPackage ./monads.nix { };
        nanosail = ocamlPackages.callPackage ./nanosail.nix { inherit monads; };
        default = ocamlPackages.callPackage ./. { inherit nanosail; };
        sail = ocamlPackages.sail;
        wrapper = pkgs.writeShellScriptBin "sail" ''
          export PATH="${pkgs.lib.makeBinPath [ pkgs.z3 ]}:$PATH"
          exec ${sail}/bin/sail --plugin ${default}/share/libsail/plugins/sail_plugin_katamaran.cmxs "$@"
        '';
      };
      devShell = pkgs.mkShell {
        # OCaml, dune, Sail and the libraries needed to build this checkout with dune build.
        # `sail` is plain Sail, not packages.wrapper: bin/sail-local and the test runner
        # add this checkout's plugin themselves, and the wrapper would load a second copy.
        inputsFrom = [ packages.default packages.nanosail packages.monads ];
        buildInputs = [
          pkgs.gcc # for cpp
          pkgs.z3
          rocqWrapped # coqc etc., seeing Katamaran, iris, stdpp and equations
          ocamlPackages.ounit2
        ];
        packages = [
          (pkgs.python3.withPackages (python-pkgs: [
            python-pkgs.psutil
          ]))
        ];
      };
    }

  );
}
