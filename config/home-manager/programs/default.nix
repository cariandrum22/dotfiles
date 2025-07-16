{ pkgs, ... }:

{
  imports = [
    ./home-manager.nix
    ./git.nix
    ./kitty.nix
    ./tmux.nix
    ./emacs.nix
    ./direnv.nix
    ./vscode
  ];
}
