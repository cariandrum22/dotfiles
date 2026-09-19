# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.139.0";
  commit = "289549a85cd652ce3513ac63044495a2ac620752";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1789751975.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1h4vhiavpih9vp6ilw1inz63552bm53ixjhlrk90qj0cyc3qq033";
    x86_64-linux = "0iqklgn0n4cys7hzzm8dgjla6lq37sj4j69vj4s7bczjh5mb9a0p";
  };
}
