args@{
  pkgs,
  ...
}:

let
  isHeadless = args ? isHeadless && args.isHeadless;
in

{
  services.gpg-agent = {
    enable = true;
    enableScDaemon = true;
    enableExtraSocket = true;
    pinentry.package =
      if pkgs.stdenv.isDarwin then
        pkgs.pinentry-qt
      else if isHeadless then
        pkgs.pinentry-curses
      else
        pkgs.pinentry-gnome3;
  };
}
