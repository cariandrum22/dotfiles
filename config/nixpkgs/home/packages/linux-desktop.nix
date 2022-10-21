{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

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
      keybase-gui
      ark
      partition-manager
      robo3t
      _1password-gui
      steam
      spotify

      # Utility
      google-drive-ocamlfuse
    ];
  };
}
