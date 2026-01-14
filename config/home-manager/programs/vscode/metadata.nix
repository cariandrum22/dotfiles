# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.109.0";
  commit = "5e5a439725cd83659b8b4ae5777b18731f0e99dc";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1768292844.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "12l5w3qddgaf97f21gc7ssdibv0lhxhv6k8hclg3ka2w93w4n68b";
    x86_64-linux = "1m4p5g0dn6fpf3946gsxpz84kkz5bzw5vh9wb94aamjf0iz5jkpw";
  };
}
