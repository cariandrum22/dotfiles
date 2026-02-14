# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.110.0";
  commit = "0366405821955682155a4507a122611c742df3fa";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1771003328.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0k6fr43b5hq2hflllrdwkm83qmqnravxa6ralaf3kzqkzjmn7f63";
    x86_64-linux = "1aqnqc1h2b4qdnvzpvc5n9dskpqqi97a0jkrxpr9ycv1vk3y46kp";
  };
}
