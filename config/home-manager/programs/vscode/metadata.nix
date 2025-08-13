# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.104.0";
  commit = "97bc9a439c022a88fba7be68f67e068361f6c467";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${{commit}}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${{commit}}/code-insider-x64-1755020201.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1qxflpwivcb6wwggwgq69ix04fypdy40zj1vf29r63pvrr9y4g8d";
    x86_64-linux = "0vraqlcw2ha03s1lms5kpfivwr2dj6z1ck44x5f5cz1w525pdcgd";
  };
}
