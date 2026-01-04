{ pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableScDaemon = true;
    enableExtraSocket = true;
    pinentry.package = if pkgs.stdenv.isDarwin then pkgs.pinentry-qt else pkgs.pinentry-gnome3;
  };
}
