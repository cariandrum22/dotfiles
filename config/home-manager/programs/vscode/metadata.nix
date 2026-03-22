# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.113.0";
  commit = "c99f8109a67528cd9ccb6de2d513bd5a7d52aa1f";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1774026501.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0n3mhjsf4xxjvszlhsrgh8yc7fsc33a1ax63l7kq10b9hc9aqcqd";
    x86_64-linux = "0d8sci1mqjsk7m3kw1239hj5dnjyai3yky2wzs9myc0v5slq7bi5";
  };
}
