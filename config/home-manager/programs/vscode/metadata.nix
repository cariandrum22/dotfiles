# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.122.0";
  commit = "da03e4ef04b9338a69f2fbb638bb173ff06471f3";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1780075063.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0f6nf5dvffc0mcg0lham01hb9lx4zwcszb8mc9pjan8kdqf6xdxs";
    x86_64-linux = "0gza9lx1gc2bfipvsz1gi8wswkglyxz802rs1h5bm0lkwgx2bic7";
  };
}
