{
  description = "trusted-publishing";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShellNoCC {
      packages = [
        nixpkgs.legacyPackages.x86_64-linux.treefmt
        nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt
        nixpkgs.legacyPackages.x86_64-linux.haskellPackages.fourmolu
        nixpkgs.legacyPackages.x86_64-linux.haskellPackages.cabal-fmt
      ];
    };
  };
}
