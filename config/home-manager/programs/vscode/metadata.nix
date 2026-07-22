# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.131.0";
  commit = "d7465d6f56c9afb8ed3696a9eaeb4f3f6d62156d";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1784653732.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0rf1aax47nm8713amca5cajfq3s46gm42yjly7amhivwjgl08wky";
    x86_64-linux = "0x30928psv62rd3i50jqjyfi28gi8q4f44njnv4mlwyhmwjqzmsr";
  };
}
