# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.114.0";
  commit = "00515ed0a37c34bc524916089b1449a7c805c325";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1774631137.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0wz1jcs4vmn0prkyniggaklfhpnn1dk0jr5d4zclq5l5smr5yq0h";
    x86_64-linux = "18w1gqmi5hh686mb3npsh52k7czd0gws373yrbn81vnp3flcncsh";
  };
}
