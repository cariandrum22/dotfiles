{ lib, pkgs, ... }:

{
  home.file = lib.mkMerge [
    {
      ".config/fish/config.fish" = {
        source = ../../fish/config.fish;
      };
      ".config/polybar" = {
        source = ../../polybar;
        recursive = true;
      };
      ".themes/Nordic" = {
        source = pkgs.fetchFromGitHub {
          owner = "EliverLara";
          repo = "Nordic";
          rev = "v2.2.0";
          sha256 = "sha256-3f8YvRzVxpZrSs8LP3H0XlEuGvmD2zpGJX6BeSqQKC4=";
        };
      };
      ".icons/candy-icons" = {
        source = pkgs.fetchFromGitHub {
          owner = "EliverLara";
          repo = "candy-icons";
          rev = "master";
          sha256 = "sha256-6aoBT8sFY5REyrJUphN0dQQlLOCJRRGfRrZU/iuOkzk=";
        };
      };
      ".config/gtk-3.0/settings.ini" = {
        source = ../../gtk-3.0/settings.ini;
      };
      ".gnupg/gpg.conf" = {
        text = ''
          list-options show-uid-validity
          verify-options show-uid-validity
          display-charset utf-8
          use-agent
          no-greeting
          require-cross-certification
          fixed-list-mode
          with-fingerprint
          with-key-origin
          personal-cipher-preferences AES256 AES192 AES
          personal-digest-preferences RIPEMD160 SHA512 SHA384 SHA256
          personal-compress-preferences ZIP ZLIB BZIP2 Uncompressed
          s2k-cipher-algo AES256
          s2k-digest-algo SHA512
          no-symkey-cache
          no-comments
          no-emit-version
        '';
      };
      ".netrc.gpg" = {
        source = ../../../netrc.gpg;
      };
      ".pythonstartup" = {
        source = ../../../pythonstartup;
      };
      ".npmrc" = {
        source = ../../../npmrc;
      };
      ".claude/settings.json" = {
        source = ../../../claude/settings.json;
      };
    }
    (lib.mkIf pkgs.stdenv.isDarwin {
      ".gnupg/scdaemon.conf".text = ''disable-ccid'';
    })
  ];
}
