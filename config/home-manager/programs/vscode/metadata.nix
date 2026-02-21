# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.110.0";
  commit = "a2860d1c314872c0c8c436647d10856fdd15f2bc";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1771607378.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0bf7nfvbjn6cg7amn1k8rl4n0fcp11rj3hf7a32pnaw7g6w1wqzv";
    x86_64-linux = "0s10cayfj455njv2kzd35jc87k5jfngw9hlhgr1288dh1bq07qkq";
  };
}
