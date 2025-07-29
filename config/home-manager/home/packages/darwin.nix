{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      fish
      bash # The version of bash preinstalled on macos is old
      pinentry_mac
      iterm2
      sequelpro
    ];
  };
}
