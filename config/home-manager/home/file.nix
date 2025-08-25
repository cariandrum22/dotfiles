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
          sha256 = "02aa1dphzjfy1fi7ln4554di24fa0kcgz8baabm15m9zm9sqfdf1";
        };
      };
      ".icons/candy-icons" = {
        source = pkgs.fetchFromGitHub {
          owner = "EliverLara";
          repo = "candy-icons";
          rev = "278998cb51c68de9d590c84d8fd1625223772792";
          sha256 = "0p12vl9d4avagl4c09nkn7cf7pif6a6qcf11wma51y18y56k0pra";
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
