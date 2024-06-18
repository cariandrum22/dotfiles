{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  home = {
    packages = with pkgs; [
      # Deep Learning
      cudatoolkit

      # Utility
      psmisc
      inotify-tools
    ];
  };
}
