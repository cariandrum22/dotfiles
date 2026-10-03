# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.141.0";
  commit = "e4685335361dd89ac2b84e47ecc64e2842fd52a9";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1790961793.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1k7n45mydv02phx7lkd6axwlc57s9k6l99mvvdbbz8siwlcr07dx";
    x86_64-linux = "1j3311b6c3y0wf0v9ml5rrw9xclb2fx705sykfk30a7hgaz7a6hq";
  };
}
