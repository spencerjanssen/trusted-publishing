{
  description = "trusted-publishing";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc912;
          devShell = {
            tools = hp: {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt
                ;
              cabal-fmt = pkgs.haskellPackages.cabal-fmt;
            };
          };
        };
      };
    };
  nixConfig = {
    extra-substituters = [ "https://sjanssen-trusted-publishing.cachix.org" ];
    extra-trusted-public-keys = [
      "sjanssen-trusted-publishing.cachix.org-1:p8MLBVXkrJI945XKQefinNic2umUPDiq9dEb5/9M9lM="
    ];
    allow-import-from-derivation = "true";
  };
}
