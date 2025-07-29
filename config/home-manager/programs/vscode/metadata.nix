# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.103.0";
  commit = "f5db653369bc87e56162be11ed3af62746347558";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1753707127.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0m50xdff96vh9v7rbl02jdh6iv8fl2yw1a419j3n1p2mc0bqgq0x";
    x86_64-linux = "0hb4bhh8an1smrnm5pq565hmkg8l8y6sf9dclmm4rlprhyp285xk";
  };
}
