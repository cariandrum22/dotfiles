{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      bash # The version of bash preinstalled on macos is old
    ];
}
