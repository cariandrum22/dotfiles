_:

let
  claudiusSource = ../../claudius;
in
{
  # Claudius has no OpenCode target yet, so link the settings directly.
  # The source lives with the other agent settings under config/claudius;
  # once Claudius supports OpenCode, deploy it via claudius.nix instead.
  xdg.configFile."opencode/opencode.jsonc".source = claudiusSource + "/opencode.settings.jsonc";
}
