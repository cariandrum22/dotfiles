{ lib, ... }:

let
  # Machine-local values for the local llama.cpp provider. They are kept out of
  # this repository and read by OpenCode through {file:...} references in
  # config/claudius/opencode.settings.json:
  #   base-url  OpenAI-compatible endpoint (for example http://host:port/v1)
  #   model-id  model identifier served by the endpoint
  #   name      display name shown by OpenCode
  localModelDir = "$HOME/.config/opencode/local-model";
in
{
  # Claudius deploys opencode.json (settings + MCP servers) and skills; see
  # programs/claudius.nix. Only the untracked local-model directory is
  # prepared here.
  home.activation.ensureOpenCodeLocalModelDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p "${localModelDir}"
    chmod 700 "${localModelDir}"
  '';
}
