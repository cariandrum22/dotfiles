{ pkgs ? import ./dep/nixpkgs {} }:
let
  inherit (import ./dep/gitignoresrc { inherit (pkgs) lib; }) gitignoreSource;
in
  pkgs.haskellPackages.developPackage {
    name = "myxmonad";
    root = gitignoreSource ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          cabal-fmt
          ormolu
          zlib
        ]);
  }
