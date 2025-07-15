# This file intentionally uses direct import to determine platform
# without requiring function arguments, maintaining compatibility
let
  pkgs = import <nixpkgs> { };
  inherit (pkgs.stdenv) isDarwin;
  base = [
    ./home-manager.nix
    ./git.nix
    ./kitty.nix
    ./tmux.nix
    ./emacs.nix
    ./direnv.nix
    ./vscode
  ];
  synthetic = if isDarwin then base else base ++ [ ./rofi.nix ];
in
{
  imports = synthetic;
}
