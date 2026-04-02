# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.115.0";
  commit = "54b695e02e47a37ded9ca68a32bb0d00d4560030";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1775067279.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0l19zfjj325asd16bynh57l6bvpkiy2wprbyhz1rc8xc54sj1322";
    x86_64-linux = "1wk4522djcm69qlsb4xzpsjbm30r172xvgpw7ld573w2s9qh11gh";
  };
}
