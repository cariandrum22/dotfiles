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
  mutableClaudiusDirs = [
    "$HOME/.config/claudius/.claude"
    "$HOME/.config/claudius/credentials/google"
    "$HOME/.config/claudius/credentials/mcp"
  ];
in
{
  xdg.configFile = {
    "claudius/bin" = {
      source = claudiusSource + "/bin";
      recursive = true;
    };
    "claudius/commands" = {
      source = claudiusSource + "/commands";
      recursive = true;
    };
    "claudius/rules" = {
      source = claudiusSource + "/rules";
      recursive = true;
    };
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

  home.activation.ensureClaudiusStateDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p ${lib.concatStringsSep " " (map lib.escapeShellArg mutableClaudiusDirs)}
    chmod 700 \
      "$HOME/.config/claudius/credentials" \
      "$HOME/.config/claudius/credentials/google" \
      "$HOME/.config/claudius/credentials/mcp"
  '';
}
