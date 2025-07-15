{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      # GUI Applications
      polybarFull
      feh
      pavucontrol
      #planify
      ledger-live-desktop
      yubioath-flutter
      remmina
      libreoffice
      google-chrome
      firefox
      keybase
      keybase-gui
      gparted
      robo3t
      _1password-gui
      spotify
      apache-directory-studio
      blueman
      system-config-printer
      simple-scan
      unstable.peazip
      pdfarranger
      gimp
      gnome-control-center

      # Utility
      trash-cli
      flameshot
      scrot
      xlsfonts
      zeal
      dupeguru

      # Game
      prismlauncher

      # Drivers
      google-drive-ocamlfuse

      # Experimental
      (callPackage ./cursor.nix { })
    ];
  };
}
