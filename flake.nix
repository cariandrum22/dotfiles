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
        packages = with pkgs; [
          zlib
          xorg.libX11
          xorg.libXext
          xorg.libXrandr
          xorg.libXScrnSaver
          alsa-lib
        ];
        haskellPackages = with pkgs.haskellPackages; [
          haskell-language-server
          ghcid
          cabal-install
          cabal-fmt
          ormolu
        ];
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = haskellPackages;
          buildInputs = packages;
        };
        formatter = pkgs.nixpkgs-fmt;
      });
}
