{ pkgs, ... }:

{
  home.packages = [ (pkgs.callPackage ./even-terminal { }) ];
}
