# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.109.0";
  commit = "f46196091d2fa0069d229cb638869b4ab95392cf";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1768567049.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1zz3kcsmhgwr6h0wxm7l3c8mq5pb47d7wwfi0xy79dvb2nn1dbbl";
    x86_64-linux = "0aw9bzxabzzn092hb93fc2x0lxqwf5j0b0wjzkwnkdhd0w9wx4ms";
  };
}
