# This file is automatically updated by the update-vscode-insiders workflow
rec {
  version = "1.105.0";
  commit = "03c265b1adee71ac88f833e065f7bb956b60550a";
  url = {
    aarch64-darwin = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/VSCode-darwin-arm64.zip";
    x86_64-linux = "https://vscode.download.prss.microsoft.com/dbazure/download/insider/${commit}/code-insider-x64-1759933367.tar.gz";
  };
  sha256 = {
    aarch64-darwin = "0fawdd0ilwq7285zdxcgk9l6k6ww6k2svq5hwi7v2lzg71m4mac8";
    x86_64-linux = "1y4hcab5gm97yjsa3bhyy7xzk3z30c43flr5v8j5pg2r3akgkha6";
  };
}
