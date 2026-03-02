# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.110.0";
  commit = "f5927e727c69039d3037f018f1790ebbbee596bc";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1772244834.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1qqw08q1iyrx1vdsaap5wjx16kll8x5g5v98lr9nqdgyh24xc9ik";
    x86_64-linux = "1iskx8nikl0hqkmhq8ibw89qlk7lkdrvlr2vvqap29hqbzxm9n9s";
  };
}
