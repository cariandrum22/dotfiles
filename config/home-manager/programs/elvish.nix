{ pkgs, lib, ... }:

let
  rivendell = pkgs.callPackage ../home/packages/rivendell.nix { };
in
{
  # Elvish configuration with custom mathematical symbol prompt
  xdg.configFile = {

    # Generate direnv integration module
    "elvish/lib/direnv.elv".text = builtins.readFile (
      pkgs.runCommand "direnv-elvish-hook" { } ''
        ${pkgs.direnv}/bin/direnv hook elvish > $out
      ''
    );

    # Local Atuin module patched for Elvish 0.21+
    "elvish/lib/atuin.elv".source = ./../../elvish/lib/atuin.elv;

    # 1Password helpers for Elvish
    "elvish/lib/onepassword.elv".source = ./../../elvish/lib/onepassword.elv;

    # Fetch spinners module from zzamboni/elvish-modules
    "elvish/lib/spinners.elv".source = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/zzamboni/elvish-modules/master/spinners.elv";
      sha256 = "0w66xh16him6p210x83v42dlb75yn9xjv0yic3bj7g1r2q9vc6gc";
    };

    # Rivendell - Functional programming library for Elvish
    "elvish/lib/rivendell".source = "${rivendell}/share/elvish/lib/rivendell";

    # fzf integration module for Elvish
    "elvish/lib/fzf.elv".source = ./../../elvish/lib/fzf.elv;

    # Nix shell integration module for Elvish
    "elvish/lib/nix-shell.elv".source = ./../../elvish/lib/nix-shell.elv;

    # Main Elvish configuration
    "elvish/rc.elv".source = ./../../elvish/rc.elv;
  };
}
