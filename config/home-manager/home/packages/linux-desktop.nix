{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

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
      steam
      spotify
      apache-directory-studio
      blueman
      system-config-printer
      gnome.simple-scan
      microsoft-edge-dev
      #peazip
      pdfarranger
      gimp

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
