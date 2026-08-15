# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.134.0";
  commit = "57bd491152b02dbf860469e75271c8e65c583ca9";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1786717062.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "03jygrjn393cbrlxk509lzc3j642wk1dpcax622rvll43wyd6krk";
    x86_64-linux = "0csnvi0z7fc6hf95x563fj9gk7dgza4bcvapvwy82w6kzsa38fc1";
  };
}
