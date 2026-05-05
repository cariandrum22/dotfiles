{
  lib,
  pkgs,
  system,
  ...
}@args:

let
  isHeadless = args ? isHeadless && args.isHeadless;
  claudiusConfig = import ../lib/claudius.nix {
    inherit lib system isHeadless;
  };
  claudiusSource = ../../claudius;
  claudiusExe = lib.getExe' pkgs.claudius "claudius";
  jsonFormat = pkgs.formats.json { };
  baseMcpServers = builtins.fromJSON (builtins.readFile (claudiusSource + "/mcpServers.json"));
  interactiveRemoteMcpServerNames = [
    "figma"
    "notion"
    "todoist"
  ];
  # These hosted servers rely on browser-based OAuth, so do not publish them on headless hosts.
  filteredMcpServers =
    if isHeadless then
      lib.filterAttrs (
        name: _: !(lib.elem name interactiveRemoteMcpServerNames)
      ) baseMcpServers.mcpServers
    else
      baseMcpServers.mcpServers;
  playwrightArgs = [
    "@playwright/mcp@latest"
  ]
  ++ lib.optionals (claudiusConfig.isLinux && !isHeadless) [
    "--executable-path=${pkgs.google-chrome}/bin/google-chrome-stable"
  ];
  managedMcpServers = baseMcpServers // {
    mcpServers = filteredMcpServers // {
      playwright = filteredMcpServers.playwright // {
        args = playwrightArgs;
      };
    };
  };
  mutableClaudiusRelativeDirs = [
    ".claude"
    "credentials"
    "credentials/google"
    "credentials/mcp"
  ];
  managedSkillSyncAgents = [
    "claude-code"
    "codex"
  ];
  managedSkillTargetRelativeDirs = [
    ".claude/skills"
    ".agents/skills"
  ];
in
{
  xdg.configFile = {
    "claudius/bin" = {
      source = claudiusSource + "/bin";
      recursive = true;
    };
    "claudius/rules" = {
      source = claudiusSource + "/rules";
      recursive = true;
    };
    # Keep only actual syncable skill directories under config/claudius/skills.
    # Design notes and catalogs belong outside that tree because Claudius treats
    # top-level Markdown files there as legacy skills during agent sync.
    "claudius/skills" = {
      source = claudiusSource + "/skills";
      recursive = true;
    };

    "claudius/claude.settings.json".source = claudiusSource + "/claude.settings.json";
    "claudius/settings.json".source = claudiusSource + "/settings.json";
    "claudius/codex.settings.toml".source = claudiusSource + "/codex.settings.toml";
    "claudius/codex.managed_config.toml".source = claudiusSource + "/codex.managed_config.toml";
    "claudius/codex.requirements.toml".source = claudiusSource + "/codex.requirements.toml";
    "claudius/gemini.settings.json".source = claudiusSource + "/gemini.settings.json";
    "claudius/mcpServers.json".source =
      jsonFormat.generate "claudius-mcpServers.json" managedMcpServers;
    "claudius/config.toml".text = claudiusConfig.claudiusConfigText;
  };

  home = {
    activation = {
      ensureClaudiusStateDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        claudius_config_dir="$HOME/.config/claudius"

        for relative_dir in ${lib.concatStringsSep " " (map lib.escapeShellArg mutableClaudiusRelativeDirs)}; do
          mkdir -p "$claudius_config_dir/$relative_dir"
        done

        chmod 700 \
          "$claudius_config_dir/credentials" \
          "$claudius_config_dir/credentials/google" \
          "$claudius_config_dir/credentials/mcp"
      '';

      pruneLegacyClaudiusSkillLayout = lib.hm.dag.entryBefore [ "linkGeneration" ] ''
        legacy_skill_root="$HOME/.config/claudius/skills"

        if [ -d "$legacy_skill_root" ]; then
          # Legacy skill layouts stored SKILL.md and templates at the skill root.
          # The current declarative tree uses skill.yaml/instructions.md plus assets/.
          find "$legacy_skill_root" -mindepth 2 -maxdepth 2 -type f \
            \( -name 'SKILL.md' -o -name '*.template' \) \
            -delete
        fi
      '';

      syncClaudiusManagedSkills = lib.hm.dag.entryAfter [ "ensureClaudiusStateDirs" ] ''
        # ~/.config/claudius/skills is the declarative source tree.
        # Agent-native skill directories remain generated artifacts.
        for relative_dir in ${lib.concatStringsSep " " (map lib.escapeShellArg managedSkillTargetRelativeDirs)}; do
          target_dir="$HOME/$relative_dir"
          if [ -d "$target_dir" ]; then
            find "$target_dir" -type f -exec chmod u+w {} +
          fi
        done

        for agent in ${lib.concatStringsSep " " (map lib.escapeShellArg managedSkillSyncAgents)}; do
          ${claudiusExe} skills sync --global --agent "$agent" --prune
        done
      '';
    };
  };
}
