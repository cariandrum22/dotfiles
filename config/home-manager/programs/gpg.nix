{ pkgs, ... }:

{
  programs.gpg = {
    enable = true;

    # Disable CCID on macOS (use system smartcard daemon instead)
    scdaemonSettings = pkgs.lib.mkIf pkgs.stdenv.isDarwin {
      disable-ccid = true;
    };

    settings = {
      # Display options
      list-options = "show-uid-validity";
      verify-options = "show-uid-validity";
      display-charset = "utf-8";
      with-fingerprint = true;
      with-key-origin = true;
      fixed-list-mode = true;
      keyid-format = "0xlong";

      # Agent and interface
      use-agent = true;
      no-greeting = true;

      # Security options
      require-cross-certification = true;
      cert-digest-algo = "SHA512";

      # Algorithm preferences (strongest first, no deprecated algorithms)
      personal-cipher-preferences = "AES256 AES192 AES";
      personal-digest-preferences = "SHA512 SHA384 SHA256";
      personal-compress-preferences = "ZIP ZLIB BZIP2 Uncompressed";
      default-preference-list = "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";

      # Key derivation settings
      s2k-cipher-algo = "AES256";
      s2k-digest-algo = "SHA512";

      # Privacy options
      no-symkey-cache = true;
      no-comments = true;
      no-emit-version = true;
      throw-keyids = true;
    };
  };
}
