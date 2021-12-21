{
  description = "conexp-clj, a general purpose software tool for Formal Concept Analysis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    gitignoresrc = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        gitignoreSource = (import inputs.gitignoresrc { inherit (pkgs) lib; }).gitignoreSource;
        conexp-clj = pkgs.callPackage ./nix/conexp-clj
          {
            inherit gitignoreSource;
          };
      in
      rec {
        packages = flake-utils.lib.flattenTree {
          conexp-clj = conexp-clj;
        };
        defaultPackage = packages.conexp-clj;
        apps.conexp-clj = flake-utils.lib.mkApp { drv = packages.conexp-clj; };
        defaultApp = apps.conexp-clj;

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            clojure-lsp
            leiningen
          ];
          shellHook = ''
            export "PATH=${conexp-clj}/bin:$PATH"
          '';
        };
      });
}
