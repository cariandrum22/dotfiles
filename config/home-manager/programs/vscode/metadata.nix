# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.138.0";
  commit = "1410ae566a7ec09ae775a65a995076f0e303fafb";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1789037157.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1nhql37j9cdqaqymil65bq0r4c8bqna6x36xs4h0l7p3iznwf70l";
    x86_64-linux = "0slaa3gdg1xsk58cc9mq1byc93l0af5ysvrwbr20324p1g6cllyr";
  };
}
