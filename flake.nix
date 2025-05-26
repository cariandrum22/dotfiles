{
  description = "A Nix flake for managing and applying my personal dotfiles.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        packages =
          with pkgs;
          [
            zlib
            (with xorg; [
              libX11
              libXext
              libXrandr
              libXScrnSaver
            ])
            (with haskellPackages; [ cabal-fmt ])
            ghcid
            cabal-install
            ormolu
            (haskell-language-server.override { supportedGhcVersions = [ "984" ]; })
            ruff
          ]
          ++ lib.optionals stdenv.isLinux [ alsa-lib ];
      in
      {
        devShell = pkgs.mkShell { buildInputs = lib.flatten packages; };
        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
