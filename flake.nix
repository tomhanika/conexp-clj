{
  description = "conexp-clj, a general purpose software tool for Formal Concept Analysis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    ...
  } @ inputs: let
    inherit (utils.lib) mkApp mkFlake;
  in
    mkFlake {
      inherit self inputs;

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      channels.nixpkgs.overlaysBuilder = channels: [inputs.clj-nix.overlays.default];

      overlays.default = final: prev: {
        inherit (self.packages."${final.system}") conexp-clj;
      };

      outputsBuilder = channels: let
        inherit (inputs.gitignore.lib) gitignoreSource;
        inherit (inputs.clj-nix.lib) mk-deps-cache;
        inherit (channels.nixpkgs) mkCljBin mkShell writeShellScriptBin;
        inherit (channels.nixpkgs.lib) pipe;

        # The web GUI (ClojureScript) compiled to a single main.js. Built as a
        # fixed-output derivation because shadow-cljs + npm fetch their own
        # dependencies from the network; the compiled output is deterministic,
        # so its hash pins the result. If the hash ever mismatches (e.g. after a
        # frontend dependency bump), nix prints the correct value to paste here.
        frontend = channels.nixpkgs.stdenv.mkDerivation {
          pname = "conexp-clj-frontend";
          version = "1";
          src = gitignoreSource ./.;
          nativeBuildInputs = [channels.nixpkgs.nodejs channels.nixpkgs.jdk21];
          buildPhase = ''
            export HOME=$TMPDIR
            npm ci
            npx shadow-cljs release app
          '';
          installPhase = ''
            mkdir -p $out
            cp src/main/resources/public/js/main.js $out/main.js
          '';
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = "sha256-Ew7Wgv7q4yfn4UGfN8sOW/+1Utc+mSson96waY1BcrU=";
        };

        pname = "conexp-clj";
        version =
          pipe ./project.clj [
            builtins.readFile
            (builtins.match ''
                .*\([[:SPACE:]]*defproject[[:SPACE:]]+${pname}[[:SPACE:]]+"([^"]+)".*'')
            builtins.head
          ];

        # Base uberjar builder. When withGui, the compiled web GUI is injected
        # into the resources so `-a` serves the browser GUI; otherwise the jar
        # serves the API only (the app shell then shows a "GUI not built"
        # notice). The default / CI build (conexp-clj) is GUI-free, so it does
        # NOT depend on the network-fetching frontend FOD and `nix flake check`
        # stays fast and green; `nix build .#conexp-clj-with-gui` opts in.
        mkConexp = {withGui}:
          mkCljBin {
            name = "conexp/${pname}";
            inherit version;

            meta = {
              description = "A General-Purpose Tool for Formal Concept Analysis";
              homepage = "https://github.com/tomhanika/conexp-clj";
              license = channels.nixpkgs.lib.licenses.epl10;
            };

            projectSrc = gitignoreSource ./.;
            main-ns = "conexp";
            jdkRunner = channels.nixpkgs.jdk21;

            buildCommand = ''
              ${channels.nixpkgs.lib.optionalString withGui ''
                mkdir -p src/main/resources/public/js
                cp ${frontend}/main.js src/main/resources/public/js/main.js
              ''}
              lein uberjar
              mkdir -p target
              cp builds/uberjar/${pname}-${version}-standalone.jar target
            '';
            doCheck = true;
            checkPhase = "lein test";
          };

        conexp = mkConexp {withGui = false;};
        conexp-with-gui = mkConexp {withGui = true;};
      in rec {
        packages = {
          conexp-clj = conexp;
          conexp-clj-with-gui = conexp-with-gui;
          conexp-clj-frontend = frontend;
          default = conexp;
        };

        apps = rec {
          deps-lock = mkApp {
            drv = writeShellScriptBin "deps-lock" ''
              ${channels.nixpkgs.deps-lock}/bin/deps-lock --lein $@
            '';
          };

          test = let
            deps = mk-deps-cache {lock-file = ./deps-lock.json;};
          in
            mkApp {
              drv = writeShellScriptBin "conexp-clj-tests" ''
                lein test $@
              '';
            };
        };

        checks = {
          inherit (packages) conexp-clj;
          devShell = devShells.default;
        };

        devShells.default = mkShell {
          buildInputs = with channels.nixpkgs; [jdk21 clojure-lsp leiningen];
        };

        formatter = channels.nixpkgs.alejandra;
      };
    };
}
# Local Variables:
# apheleia-formatter: alejandra
# End:

