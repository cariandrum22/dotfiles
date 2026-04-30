{
  lib,
  system,
  ...
}@args:

let
  isDarwin = lib.hasSuffix "-darwin" system;
  isLinux = lib.hasSuffix "-linux" system;
  isHeadless = args ? isHeadless && args.isHeadless;
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
  home.sessionVariables = lib.optionalAttrs (onepasswordMode != null) {
    CLAUDIUS_1PASSWORD_MODE = onepasswordMode;
    CLAUDIUS_1PASSWORD_VAULT = onepasswordVault;
    CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH = serviceAccountTokenEnvPath;
  };

  xdg.configFile = lib.optionalAttrs (onepasswordMode != null) {
    "claudius/config.toml" = {
      force = true;
      text = claudiusConfigText;
    };
  };
}
