{ config, lib, ... }:

let
  pkgs = import <nixpkgs> { };
  isDarwin = pkgs.stdenv.isDarwin;
  base = [
    ./home
    ./programs
  ];
  linux = base ++ [
    ./xsession.nix
    ./services/picom.nix
    ./services/dunst.nix
    ./services/keybase.nix
    ./services/vscode-server.nix
    ./services/gpg-agent.nix
  ];
  synthetic = if isDarwin then base else linux;
in
{
  home = {
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";
    stateVersion = "25.05";
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        unstable = import <unstable> { config = config.nixpkgs.config; };
      };
    };
    overlays = [ (import ./overlays/xmonad.nix) ];
  };

  imports = synthetic;

  xresources.properties = {
    "urxvt*foreground" = "#d3d3d3";
    "urxvt*background" = "#394939";
    "urxvt*color0" = "#333333";
    "urxvt*color1" = "#cc827b";
    "urxvt*color2" = "#99e699";
    "urxvt*color3" = "#cccc66";
    "urxvt*color4" = "#9999e6";
    "urxvt*color5" = "#e666e6";
    "urxvt*color6" = "#99cce6";
    "urxvt*color7" = "#e6e6e6";
    "urxvt*color8" = "#444444";
    "urxvt*color9" = "#e68686";
    "urxvt*color10" = "#66e666";
    "urxvt*color11" = "#e6e666";
    "urxvt*color12" = "#b6b6e6";
    "urxvt*color13" = "#e686e6";
    "urxvt*color14" = "#66e6e6";
    "urxvt*color15" = "#e6e6e6";
  };

  # NOTE: macOS applications installed by home-manager cannot be launched
  # by Spotlight.
  # Because Spotlight does not display symbolic links in the GUI.
  # This issue is already listed in GitHub Issues, and the temporal solution
  # to copy the Applications under $HOME/Applications is the one described
  # in the issue comment.
  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-778820334
  home.activation = lib.mkIf isDarwin {
    copyApplications =
      let
        apps = pkgs.buildEnv {
          name = "home-manager-applications";
          paths = config.home.packages;
          pathsToLink = "/Applications";
        };
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        baseDir="$HOME/Applications/Home Manager Apps"
        if [ -d "$baseDir" ]; then
          rm -rf "$baseDir"
        fi
        mkdir -p "$baseDir"
        for appFile in ${apps}/Applications/*; do
          target="$baseDir/$(basename "$appFile")"
          $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
          $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
        done
      '';
  };
  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-1301555596
  disabledModules = [ "targets/darwin/linkapps.nix" ];
}
