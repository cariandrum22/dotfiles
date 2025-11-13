# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.107.0";
  commit = "ae900a8cf53cf62398a04036dc45134ef7f0613a";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1762925109.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1s4i4sbwbl1z0xdvk7c3fsknzanskgpgx4bfapzhyr65ljis3mgn";
    x86_64-linux = "0ylg18bmaq2440zysczlh3rm4b7plij2ihgzvbx9m5qimrkn6pm6";
  };
}
