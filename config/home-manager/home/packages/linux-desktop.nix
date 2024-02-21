{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  home = {
    packages = with pkgs; [
      # Deep Learning
      cudatoolkit

      # GUI Applications
      polybarFull
      feh
      pavucontrol
      #elementary-planner NOTE: This package is currently broken
      ledger-live-desktop
      yubikey-manager-qt
      remmina
      libreoffice
      google-chrome
      firefox
      keybase
      keybase-gui
      partition-manager
      robo3t
      _1password-gui
      steam
      spotify
      apache-directory-studio
      blueman
      system-config-printer
      gnome.simple-scan
      microsoft-edge-dev
      peazip

      # Utility
      trash-cli
      flameshot
      xlsfonts
      zeal

      # Game
      prismlauncher

      # Drivers
      google-drive-ocamlfuse
      xboxdrv
    ];
  };
}
