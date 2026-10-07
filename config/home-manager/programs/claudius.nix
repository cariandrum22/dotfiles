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
  tomlFormat = pkgs.formats.toml { };
  baseMcpServers = builtins.fromJSON (builtins.readFile (claudiusSource + "/mcpServers.json"));
  nonHeadlessMcpServerNames = [
    "figma"
    "notion"
    "todoist"
  ];
  # These servers either need a local desktop app or browser-mediated
  # authentication, so do not publish them on headless hosts.
  filteredMcpServers =
    if isHeadless then
      lib.filterAttrs (name: _: !(lib.elem name nonHeadlessMcpServerNames)) baseMcpServers.mcpServers
    else
      baseMcpServers.mcpServers;
  playwrightArgs =
    filteredMcpServers.playwright.args
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
  baseCodexSettings = builtins.fromTOML (builtins.readFile (claudiusSource + "/codex.settings.toml"));
  # Policy-only tables must not reintroduce servers omitted on headless hosts.
  managedCodexSettings = baseCodexSettings // {
    mcp_servers = lib.filterAttrs (
      name: _: builtins.hasAttr name filteredMcpServers
    ) baseCodexSettings.mcp_servers;
  };
  mutableClaudiusRelativeDirs = [
    ".claude"
    "credentials"
    "credentials/google"
    "credentials/mcp"
    "credentials/mcp/brave-search"
    "credentials/mcp/github"
    "credentials/mcp/google-personal"
    "credentials/mcp/google-workspace"
    "credentials/mcp/x"
  ];
  managedSkillSyncAgents = [
    "antigravity"
    "claude-code"
    "codex"
    "opencode"
  ];
  managedSkillTargetRelativeDirs = [
    ".gemini/config/skills"
    ".claude/skills"
    ".agents/skills"
    ".config/opencode/skills"
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

    # Antigravity CLI reads ~/.gemini/antigravity-cli/settings.json; Claudius
    # merges this source there on `config sync --global --agent antigravity`.
    "claudius/antigravity.settings.json".source = claudiusSource + "/antigravity.settings.json";
    "claudius/claude.settings.json".source = claudiusSource + "/claude.settings.json";
    "claudius/codex.settings.toml".source =
      tomlFormat.generate "claudius-codex.settings.toml" managedCodexSettings;
    "claudius/codex.managed_config.toml".source = claudiusSource + "/codex.managed_config.toml";
    "claudius/codex.requirements.toml".source = claudiusSource + "/codex.requirements.toml";
    # The local model endpoint and name stay out of this file; OpenCode reads
    # them from ~/.config/opencode/local-model (see programs/opencode.nix).
    # The provider timeout bounds a single inference request; raise it if the
    # local model legitimately needs longer.
    "claudius/opencode.settings.json".source = claudiusSource + "/opencode.settings.json";
    "claudius/mcpServers.json".source =
      jsonFormat.generate "claudius-mcpServers.json" managedMcpServers;
    "claudius/config.toml".text = claudiusConfig.claudiusConfigText;
  };

  home = {
    packages = [ pkgs.github-mcp-server ];

    activation = {
      ensureClaudiusStateDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        claudius_config_dir="$HOME/.config/claudius"

        for relative_dir in ${lib.concatStringsSep " " (map lib.escapeShellArg mutableClaudiusRelativeDirs)}; do
          mkdir -p "$claudius_config_dir/$relative_dir"
        done

        chmod 700 \
          "$claudius_config_dir/credentials" \
          "$claudius_config_dir/credentials/google" \
          "$claudius_config_dir/credentials/mcp" \
          "$claudius_config_dir/credentials/mcp/brave-search" \
          "$claudius_config_dir/credentials/mcp/github" \
          "$claudius_config_dir/credentials/mcp/google-personal" \
          "$claudius_config_dir/credentials/mcp/google-workspace" \
          "$claudius_config_dir/credentials/mcp/x"
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
