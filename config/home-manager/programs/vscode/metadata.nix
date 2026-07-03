# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.128.0";
  commit = "2be9624b801009a53db8f629bd7762137d100007";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1783012278.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1fainhxq9dx1b3a06brpsj7dhc62a8i1gnhw1lf1h41sfr7y0wr3";
    x86_64-linux = "19ds7ill8310rcbsfn1f17y9dnff8cfq01b4jl8laicdy90x6ixq";
  };
}
