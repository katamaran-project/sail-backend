{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    # https://github.com/NixOS/nixpkgs/pull/387197
    nixpkgs.url = "github:NixOS/nixpkgs/b6fd4a67122a1477291b4c3e4fb8f6794ac982b3";
    katamaran = {
      url = "github:katamaran-project/katamaran";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
    in
    rec {
      packages = rec {
        monads = pkgs.ocamlPackages.callPackage ./monads.nix { };
        nanosail = pkgs.ocamlPackages.callPackage ./nanosail.nix { inherit monads; };
        default = pkgs.ocamlPackages.callPackage ./. { inherit nanosail; };
        sail = pkgs.ocamlPackages.sail;
        wrapper = pkgs.writeShellScriptBin "sail" ''
          export PATH="${pkgs.lib.makeBinPath [ pkgs.z3 ]}:$PATH"
          exec ${sail}/bin/sail --plugin ${default}/share/libsail/plugins/sail_plugin_katamaran.cmxs "$@"
        '';
      };
      devShell = pkgs.mkShell {
        buildInputs = [
          packages.wrapper
          pkgs.z3
          inputs.katamaran.packages.${system}.coq819
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

