{
  lib,
  system,
  ...
}@args:

let
  claudiusConfig = import ../lib/claudius.nix {
    inherit lib system;
    isHeadless = args ? isHeadless && args.isHeadless;
  };
in
{
  home.sessionVariables = lib.optionalAttrs (claudiusConfig.onepasswordMode != null) {
    CLAUDIUS_1PASSWORD_MODE = claudiusConfig.onepasswordMode;
    CLAUDIUS_1PASSWORD_VAULT = claudiusConfig.onepasswordVault;
    CLAUDIUS_1PASSWORD_SERVICE_ACCOUNT_TOKEN_PATH = claudiusConfig.serviceAccountTokenEnvPath;
  };
}
