# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.140.0";
  commit = "9cc2a44bf20ac6f4badef53bc9f33dfdb5803378";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1790140251.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0i39lk774phqd9xkvyc3ijyp58q3pcq4w70kmrpkqfnhnq2mvs4r";
    x86_64-linux = "0lmr0v1hkj7kvbi524a74562mlfxckl93ccnrjrbq0fr08hwv49y";
  };
}
