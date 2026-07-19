# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.130.0";
  commit = "5e212606d57c0a17f73bc8b7ae0cc9e14bcfd345";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1784306972.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "06j3p97c6jrg6zil4d82fn0kj11cxxx8cl3q24ssjvwk2y011a4y";
    x86_64-linux = "1lj1i0g05wzkvh8qd6qpjswb8934yy6vmbdc55fn7c0vca2y5195";
  };
}
