{
  config,
  lib,
  pkgs,
  ...
}:

{
  # NOTE: macOS applications installed by home-manager cannot be launched
  # by Spotlight.
  # Because Spotlight does not display symbolic links in the GUI.
  # This issue is already listed in GitHub Issues, and the temporal solution
  # to copy the Applications under $HOME/Applications is the one described
  # in the issue comment.
  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-778820334
  home.activation = {
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
