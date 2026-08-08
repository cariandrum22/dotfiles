# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.133.0";
  commit = "8eaafbf4220a3cb099d7bd99c8d04dca7062236f";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1786123385.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "091abvcc3zbic956w8j4scbdf9k3i774l3cykqz62fxq84iw4v3p";
    x86_64-linux = "1g74yyvpg631sdsbkh4rcjhvkbhg9p4kia1mzhkzlgf41apw1lz6";
  };
}
