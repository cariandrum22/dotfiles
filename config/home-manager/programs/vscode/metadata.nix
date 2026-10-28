# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.145.0";
  commit = "a3797664749856829367927a93d65834507e4d12";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1793121755.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "05w5570xvvyqdqgj527h3bv08642wqgz2z7xwxmb48vq3b8i5vm4";
    x86_64-linux = "10k2b6b43rbwca6r1b264k3vh1skhihz7dlkrh8lazcry3wjknai";
  };
}
