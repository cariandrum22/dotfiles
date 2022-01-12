{ pkgs, ... }:

let
  pinentry-program = if builtins.currentSystem == "x86_64-darwin" then
    "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
  else
    "${pkgs.pinentry_gnome}/bin/pinentry-gnome3";
in {
  home.file = {
    ".config/fish/config.fish" = { source = ../../fish/config.fish; };
    ".config/fish/fishfile" = { source = ../../fish/fishfile; };
    ".config/polybar" = {
      source = ../../polybar;
      recursive = true;
    };
    ".themes/Nordic" = {
      source = builtins.fetchGit {
        url = "https://github.com/EliverLara/Nordic";
        ref =  "refs/tags/v2.1.0";
      };
    };
    ".icons/candy-icons" = {
      source = builtins.fetchGit {
        url = "https://github.com/EliverLara/candy-icons";
      };
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
    ".gnupg/gpg-agent.conf" = {
      text = ''
        pinentry-program ${pinentry-program}
      '';
    };
    ".netrc.gpg" = { source = ../../../netrc.gpg; };
    ".pythonstartup" = { source = ../../../pythonstartup; };
  };
}
