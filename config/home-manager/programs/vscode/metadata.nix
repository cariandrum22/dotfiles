# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.148.0";
  commit = "a0be5bebd4de2e0f08408e135abd93881d7f00df";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1795124869.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1vawbnamdk53rf3wgz0c0ymzc693jxj1d2pi9bi1ric92wsyp5m3";
    x86_64-linux = "11j0bvg2dahddrqk40dkdjfmlca2a0ckxwb3m51hnxrxn5ab5qcg";
  };
}
