{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      # Deep Learning
      cudatoolkit

      # Utility
      psmisc
      inotify-tools
      zip
      squashfsTools
    ];
  };
}
