# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.140.0";
  commit = "4a6a3612a562c50f54c2bd5211f0ea42915b974e";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1790334839.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0r5ky7zgnxap7vfkai60f24ljdf10cvdxpagr7ay0vhs3cz4wzs4";
    x86_64-linux = "1sd56v5piwzwc78qamzcq7jw07bcmsd60zrir6b58km9nv0lsssm";
  };
}
