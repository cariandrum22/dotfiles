let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <unstable> { config.allowUnfree = true; };

in {
  home = {
    packages = with pkgs; [
      # Deep Learning
      cudatoolkit
      cudnn

      # GUI Applications
      polybarFull
      dmenu
      rofi
      feh
      pavucontrol
      elementary-planner
      xmind
      ledger-live-desktop
      yubikey-manager-qt
      remmina
      libreoffice
      google-chrome
      firefox
      keybase
      ark
      partition-manager
      slack
      robo3t
      teams
      _1password-gui
      steam
      wireshark
      spotify

      # Utility
      google-drive-ocamlfuse
      yubikey-manager

      # DevOps
      nixops

      # Development Environment
      unstable.vscode
      jetbrains.datagrip

      # Compiler and Runtime
      jdk
    ];
  };
}
