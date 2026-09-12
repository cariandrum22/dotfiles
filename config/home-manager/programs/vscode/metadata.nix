# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.138.0";
  commit = "60ac69b364809ee6976d6dc3ed74cc7628f637da";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1789113002.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "00m0d4225pi44yjxby2rym1mzv1zkzrb5m81zicjswlv6kc9smfz";
    x86_64-linux = "1k7pn2v18dpwidzyzd5a336a1z0v28v59wcllyw27m3plqh5lnkj";
  };
}
