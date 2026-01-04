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
      ".netrc.gpg" = {
        source = ../../../netrc.gpg;
      };
      ".pythonstartup" = {
        source = ../../../pythonstartup;
      };
      ".npmrc" = {
        source = ../../../npmrc;
      };
      # tmux clipboard helper script
      ".local/bin/tmux-clipboard-helper" = {
        executable = true;
        text = ''
          #!/usr/bin/env sh
          # Dynamic DISPLAY detection for tmux clipboard operations

          # Try to get DISPLAY from systemd (Linux)
          if command -v systemctl >/dev/null 2>&1; then
              DISPLAY_VAR=$(systemctl --user show-environment 2>/dev/null | grep '^DISPLAY=' | cut -d= -f2)
          fi

          # Fallback to environment or :0
          DISPLAY=''${DISPLAY_VAR:-''${DISPLAY:-:0}}
          export DISPLAY

          # Execute the command passed as arguments
          exec "$@"
        '';
      };
    }
  ];
}
