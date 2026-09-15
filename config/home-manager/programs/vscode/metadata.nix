# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.138.0";
  commit = "1d01c8df715905edfad1c49935680e8156358021";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1789422264.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1a5pvjs5kqm1xala77n65mbkclzxja6av68gnacmiwadzknmgw8d";
    x86_64-linux = "1nyhvnr43h77pal3g1wfz95fys7n22hilf96i6lh336kj49qgrdp";
  };
}
