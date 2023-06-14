let
  isDarwin = (import <nixpkgs> { }).stdenv.isDarwin;
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
