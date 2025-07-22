# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.103.0";
  commit = "ec0f0be4a71892d46a136811f5a5c160f032f129";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1753075027.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1sy9c8sg6lczcspg6mgadabsmgc4ilakb5bix1i5qgr0ir5rbf6d";
    x86_64-linux = "1p8lc855b329p5ya1zc083kqxqxkb1c6ggmhm39p5l37mlkvxs6s";
  };
}
