{
  description =
    "conexp-clj, a general purpose software tool for Formal Concept Analysis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    clj-nix = {
      #url = "github:jlesquembre/clj-nix";
      url =
        "github:mmarx/clj-nix/fix-lein"; # we need to wait for PR 31 to go through.
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils/flake-utils";
      };
    };

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, utils, ... }@inputs:
    let inherit (utils.lib) mkApp mkFlake;
    in mkFlake {
      inherit self inputs;

      channels.nixpkgs.overlaysBuilder = channels:
        [ inputs.clj-nix.overlays.default ];

      outputsBuilder = channels:
        let
          inherit (inputs.gitignore.lib) gitignoreSource;
          inherit (channels.nixpkgs) mkCljBin mkShell writeShellScriptBin;

          conexp = let
            pname = "conexp-clj";
            version = "2.3.0-SNAPSHOT";
          in mkCljBin rec {
            name = "conexp/${pname}";
            inherit version;

            projectSrc = gitignoreSource ./.;
            main-ns = "conexp";

            buildCommand = ''
              lein uberjar
              mkdir -p target
              cp builds/uberjar/${pname}-${version}-standalone.jar target
            '';
            doCheck = true;
            checkPhase = "lein test";
          };

        in {
          packages = rec {
            conexp-clj = conexp;
            default = conexp-clj;
          };

          apps = rec {
            conexp-clj = mkApp { drv = conexp; };
            default = conexp-clj;
            deps-lock = mkApp {
              drv = writeShellScriptBin "deps-lock" ''
                ${channels.nixpkgs.deps-lock}/bin/deps-lock --lein $@
              '';
            };
          };

          devShells.default = mkShell {
            buildinputs = with channels.nixpkgs; [ clojure-lsp leiningen ];
          };

          formatter = channels.nixpkgs.alejandra;

        };

    };
}
