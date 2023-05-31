{ pkgs, ... }:

let
  unstable = import <unstable> { };
in
{
  home.packages = with pkgs; [
    binutils # cabal needs`ar`
    ghc
    cabal-install
    stack
    ormolu
    hlint
    haskell-language-server
    haskellPackages.hoogle
    haskellPackages.ghcide
    haskellPackages.cabal-fmt
  ];

  nixpkgs.overlays = [
    (self: super: {
      haskell-language-server = unstable.haskell-language-server.override {
        supportedGhcVersions = [ "927" ];
      };
    })
  ];

  home.file.".ghci".source = ../../../../ghci;

  programs.git.ignores = [
    "dist"
    "dist-*"
    "cabal-dev"
    "*.oev"
    "*.hiev"
    "*.hie"
    "*.chi"
    "*.chs.h"
    "*.dyn_o"
    "*.dyn_hi"
    ".hpc"
    ".hsenv"
    ".cabal-sandbox/"
    "cabal.sandbox.config"
    "*.prof"
    "*.aux"
    "*.hp"
    "*.eventlog"
    ".stack-work/"
    "cabal.project.local"
    "cabal.project.local~"
    ".HTF/ev"
    ".ghc.environment.*"
  ];
}
