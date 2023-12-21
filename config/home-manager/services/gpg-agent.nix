{ pkgs, ... }:

{
  services.gpg-agent =
    {
      enable = true;
      enableScDaemon = true;
      pinentryFlavor = if pkgs.stdenv.isDarwin then "qt" else "gnome3";
    };
}
