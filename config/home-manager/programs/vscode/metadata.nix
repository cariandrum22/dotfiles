# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.104.0";
  commit = "2c749892adf66679e282a1b27ae526cf995d6f0c";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1754630685.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0dssvdlpim0afnwk28k906y45zvx8z0izs2ssc07pv8pqn4dfk0j";
    x86_64-linux = "0s5kddqqlgfp8xhcjbvgpi6mff6v1x57sqaasdj8zd8j6sda7x3l";
  };
}
