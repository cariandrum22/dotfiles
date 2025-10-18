# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.106.0";
  commit = "ec8586ef251f560f4deaaa205d67bc32213b5055";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1760679998.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "1lv76y30hqlz304ydmsf50ax9b7sszqx9higb9yy8nkw2bjrlcp8";
    x86_64-linux = "1r04vsjhvc3r1b3l10igr9zcy8is1g7ysr88r8b10l1zm504p62j";
  };
}
