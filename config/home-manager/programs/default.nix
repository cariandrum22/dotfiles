{ ... }:

{
  imports = [
    ./home-manager.nix
    ./git.nix
    ./gpg.nix
    ./kitty.nix
    ./tmux.nix
    ./emacs.nix
    ./direnv.nix
    ./atuin.nix
    ./elvish.nix
    ./vscode
  ];
}
