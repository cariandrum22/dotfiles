# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.112.0";
  commit = "4c822235d64e2486552fd52187d1b2a29fa8f4f1";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1773444957.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "09prn5j9bi4js1s0arq5l4zfirx61p0ckxpl5gvbcgczy44jpfxz";
    x86_64-linux = "05yz5213nwbkqmlndawa8q041xjxnq2qf7s4r8w6jy8ci9xsl7nm";
  };
}
