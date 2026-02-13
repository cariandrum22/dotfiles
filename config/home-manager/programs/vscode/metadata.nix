# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.110.0";
  commit = "cc318781a13d6869a36573f9de9eabacc9fad49f";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1770923378.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1qa36j99r8m03i3f4l90jjfadrd0pwqasimlh0xhi35r5s5xfcmp";
    x86_64-linux = "01yga9n1alhmx76jlws8i98qi8whimbb44l08gwh92s09als1lhb";
  };
}
