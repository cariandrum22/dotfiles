# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.107.0";
  commit = "b2426c57053497e6c1eea406b8f2855e442992b4";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1763733071.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0x231h0sfhx5bg8m0vdbbhip6ap3w8gvbvm8cq3qnn8747bx0c8r";
    x86_64-linux = "1c4md9hnvqqyv8q240f3a44xbasvlm46lk0gm363gzhqrg0pfn8a";
  };
}
