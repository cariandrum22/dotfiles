{ ... }:

{
  imports = [
    ./home-manager.nix
    ./git.nix
    ./kitty.nix
    ./tmux.nix
    ./emacs.nix
    ./direnv.nix
    ./atuin.nix
    ./elvish.nix
    ./vscode
  ];
}
