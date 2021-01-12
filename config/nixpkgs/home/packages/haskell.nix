{ pkgs, ... }:

{
  home.packages = with pkgs; [
    binutils # cabal needs`ar`
    ghc
    cabal-install
    stack
    ormolu
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
