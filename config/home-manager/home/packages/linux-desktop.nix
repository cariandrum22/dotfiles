{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      # GUI Applications
      polybarFull
      feh
      pavucontrol
      planify
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
      spotify
      apache-directory-studio
      blueman
      system-config-printer
      gnome.simple-scan
      microsoft-edge-dev
      unstable.peazip
      pdfarranger
      gimp
      gnome.gnome-control-center

      # Utility
      trash-cli
      flameshot
      xlsfonts
      zeal
      dupeguru

      # Game
      prismlauncher

      # Drivers
      google-drive-ocamlfuse
      xboxdrv
    ];
  };
}
