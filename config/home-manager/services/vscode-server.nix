{ nixos-vscode-server, ... }:

{
  imports = [
    nixos-vscode-server.homeModules.default
  ];

  services.vscode-server = {
    enable = true;
    installPath = "$HOME/.vscode-server-insiders";
  };
}
