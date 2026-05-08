# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.120.0";
  commit = "ca2ded47a87743fae81319a26b68f33cf1043998";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1778174378.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0lv85r42khbnvqdc7qyxhyambj1rc8bfbczmbgvi6lq04f6kb9sb";
    x86_64-linux = "1aw23p7s0dab9pc0il9m0ga06zk0044ar0yvif7plazjd3yl59np";
  };
}
