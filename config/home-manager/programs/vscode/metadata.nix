# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.104.0";
  commit = "06acd067cb9621b055d9701324477cf75fa0e242";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1757067327.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "12rz1v04d63x5l5dpikbvw5az1wc8wj6n7cxq00pi29hd0k28ybv";
    x86_64-linux = "11rfjynv8ihsvn7h9rqayyq2rwg893c4r83w4rc67j1mgmmlzzns";
  };
}
