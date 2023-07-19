{
  description =
    "conexp-clj, a general purpose software tool for Formal Concept Analysis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    clj-nix = {
      url = "github:jlesquembre/clj-nix";
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

      overlays.default = final: prev: {
        inherit (self.packages."${final.system}") conexp-clj;
      };

      outputsBuilder = channels:
        let
          inherit (inputs.gitignore.lib) gitignoreSource;
          inherit (inputs.clj-nix.lib) mk-deps-cache;
          inherit (channels.nixpkgs) mkCljBin mkShell writeShellScriptBin;
          inherit (channels.nixpkgs.lib) pipe;

          conexp = let
            versionFromDefproject = name:
              pipe ./project.clj [
                builtins.readFile
                (builtins.match ''
                  .*\([[:SPACE:]]*defproject[[:SPACE:]]+${name}[[:SPACE:]]+"([^"]+)".*'')
                builtins.head
              ];
            pname = "conexp-clj";
          in mkCljBin rec {
            name = "conexp/${pname}";
            version = versionFromDefproject pname;

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

            test = let deps = mk-deps-cache { lock-file = ./deps-lock.json; };
            in mkApp {
              drv = writeShellScriptBin "conexp-clj-tests" ''
                lein test $@
              '';
            };
          };

          devShells.default = mkShell {
            buildInputs = with channels.nixpkgs; [ clojure-lsp leiningen ];
          };

          formatter = channels.nixpkgs.alejandra;

        };

    };
}
