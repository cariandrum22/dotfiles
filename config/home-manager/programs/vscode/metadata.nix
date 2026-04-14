# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.116.0";
  commit = "5272631ea6da49bc305e7264cbb39190604d9f40";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1776124930.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1f62n0hzi7qazbfck55azh04wqm1y0llvyly6q51pwbw3dh7wd3k";
    x86_64-linux = "1my4jbp53ndmaxpws1xfy2ych104v7m4g46cjzdhr2dm2mfmn7y1";
  };
}
