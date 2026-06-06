# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.124.0";
  commit = "0035c783eccf8a2efbdb972dfe62becb51f11fc6";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1780668591.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0x9k7l4f12w0iln2rd9akkpzs8i7zgh0x3350fwjqd0gsjbfxbdw";
    x86_64-linux = "1mql3i1njq7klm58p6aird4k2p3xcnbzgrcs79mx648b8nyz6f6y";
  };
}
