{
  description = "dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
            haskellPackages.cabal-install
            haskellPackages.cabal-fmt
            haskellPackages.ormolu
            zlib
            xlibsWrapper
            xorg.libXrandr
            xorg.libXScrnSaver
          ];
        };
      });
}
