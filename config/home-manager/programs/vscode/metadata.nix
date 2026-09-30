# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.140.0";
  commit = "b6c7324cedb6fc4ed639e3c7e1d4b233a09df8bc";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1790714900.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "01dvhk4jby2dmhpd08yqg61ik45dcdcqza9hrabgdwh5k1c2pxmp";
    x86_64-linux = "11v7ni4h8an13xdhxz397zq3a1lg2vd6cq3f9zi6ychy7awv0x34";
  };
}
