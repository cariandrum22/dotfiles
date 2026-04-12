# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.116.0";
  commit = "47add6dfa59e9d89115b53c74511f03db9575dd0";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1775822703.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "03lnfpb6gllsmr0pgz9lanm2im75c5p9nlca1hk0d63f9qda1fjh";
    x86_64-linux = "0821f8n63yw1prrskbigcqif4lxc0nvav7pzaf3mqz91jhjm75na";
  };
}
