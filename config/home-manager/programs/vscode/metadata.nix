# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.135.0";
  commit = "7e0ab9d1672db1f56f2a67913f4df63a609a2119";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1787177087.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "06z4fgcih37dip6297r8ipr5b6px082r02q0ifmrl8pbikjmc7wp";
    x86_64-linux = "189pw574y207v5xx7ab7q21vm19kxpv8pjmf4vxscp47ibpyd1gg";
  };
}
