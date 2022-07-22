let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <unstable> { config.allowUnfree = true; };

in {
  home = {
    packages = with pkgs; [
      # Deep Learning
      cudatoolkit
      cudaPackages.cudnn

      # GUI Applications
      polybarFull
      dmenu
      rofi
      feh
      pavucontrol
      elementary-planner
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
      keybase-gui
      zoom-us

      # Utility
      google-drive-ocamlfuse
      yubikey-manager

      # Development Environment
      unstable.vscode
      jetbrains.datagrip
    ];
  };
}
