# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.134.0";
  commit = "42b1420914c8b46f690e8b5f385722320d7b2d8f";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1786383371.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0djs7dvl24nz85dh9bs21ar49pjiby8cflr4k2qfzf7jrjp72hz7";
    x86_64-linux = "1dzw01zl95db1ck4b0cj4j51azhmk91nxl3iahpxsraprbqbyfb0";
  };
}
