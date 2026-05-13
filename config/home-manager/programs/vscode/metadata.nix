# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.120.0";
  commit = "0958016b2af9f09bb4257e0df4a95e2f90590f9f";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1778619079.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "061gha0n0cr4jcr9xipqn6d0k0pzvzs21phv7rm1idkf1zrl8n2i";
    x86_64-linux = "1qfyl2yg5cdsrcww0cia0s4qn7qfg8vhjs003kg70c4fqjclakym";
  };
}
