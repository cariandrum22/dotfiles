# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.109.0";
  commit = "108524fc6e063e911782cf4240a4065ffac66d24";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1769122469.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0dyr1q7c3w62cyvn08a78dnjn5lcqr18sa9nnc65a2ppw8ad709r";
    x86_64-linux = "1k66x51qn7zr1x8nkq80cqc8xnbcayzn57wyqzw4ikas26gicp6l";
  };
}
