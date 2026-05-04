{
  lib,
  system,
  isHeadless ? false,
}:

let
  isDarwin = lib.hasSuffix "-darwin" system;
  isLinux = lib.hasSuffix "-linux" system;
  serviceAccountTokenEnvPath = "$HOME/.config/op/service-accounts/headless-linux-cli.token";
  serviceAccountTokenConfigPath = "~/.config/op/service-accounts/headless-linux-cli.token";
  onepasswordMode =
    if isDarwin then
      "desktop"
    else if isLinux && isHeadless then
      "service-account"
    else if isLinux then
      "manual"
    else
      null;
  onepasswordVault = if onepasswordMode != null then "Automation" else null;
  claudiusConfigText = lib.concatStrings [
    ''
      # Managed by Home Manager. Edit dotfiles instead of this file.
      [default]
      agent = "claude"
      context-file = "CLAUDE.md"

      [secret-manager]
      type = "1password"

      [secret-manager.onepassword]
      mode = "${onepasswordMode}"
    ''
    (lib.optionalString (isLinux && isHeadless) ''
      service-account-token-path = "${serviceAccountTokenConfigPath}"
    '')
  ];
in
{
  inherit
    isDarwin
    isLinux
    serviceAccountTokenEnvPath
    serviceAccountTokenConfigPath
    onepasswordMode
    onepasswordVault
    claudiusConfigText
    ;
}
