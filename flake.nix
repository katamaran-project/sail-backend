{
  nixConfig = {
    extra-substituters = [ "https://katamaran.cachix.org" ];
    extra-trusted-public-keys = [ "katamaran.cachix.org-1:6oOkbLd/LRTGiixl0cVyWC2/OXo5A0l1e84jqZf+dCE=" ];
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    # https://github.com/NixOS/nixpkgs/pull/387197
    nixpkgs.url = "github:NixOS/nixpkgs/b6fd4a67122a1477291b4c3e4fb8f6794ac982b3";
    katamaran = {
      # Pinned to before callgraph were introduced.
      url = "github:katamaran-project/katamaran/d744bdc78e9660e4a2776cb2516469bc7dd4d4ce";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      rocqPackages = pkgs.coqPackages_8_20.overrideScope (self: super:
        { katamaran = self.callPackage inputs.katamaran { }; }
      );
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
          rocqPackages.coq
          rocqPackages.katamaran
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

