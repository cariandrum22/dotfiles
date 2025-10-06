{ pkgs, lib, ... }:

{
  # Atuin - Magical shell history
  # Supports Fish, Bash, Zsh, and can be integrated with Elvish via mellon
  programs.atuin = {
    enable = true;

    settings = {
      # Database and sync settings
      auto_sync = true;
      sync_frequency = "5m";
      sync_address = "https://api.atuin.sh";

      # Search settings
      search_mode = "fuzzy"; # Ctrl-R uses fuzzy search
      search_mode_shell_up_key_binding = "prefix"; # Up arrow uses prefix (forward) match
      filter_mode = "global";
      style = "auto";
      inline_height = 20;

      # UI settings
      show_preview = true;
      show_help = true;
      exit_mode = "return-query";

      # History settings
      history_filter = [
        "^secret"
        "^token"
        "^password"
        "^API_KEY"
        "^AWS_SECRET"
      ];

      # Update check
      update_check = true;
    };

    # Fish integration
    enableFishIntegration = true;

    # Bash integration (if needed)
    enableBashIntegration = false;

    # Zsh integration (if needed)
    enableZshIntegration = false;
  };

  # Additional configuration file for Atuin
  xdg.configFile."atuin/config.toml".text = lib.mkAfter ''
    # Additional Atuin settings
    # Import Fish history automatically on first run
    # Run: atuin import auto
  '';
}
