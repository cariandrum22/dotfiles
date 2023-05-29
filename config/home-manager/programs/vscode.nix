let
  unstable = import <unstable> { config.allowUnfree = true; };
in
{
  programs.vscode = {
    enable = true;
    package = unstable.vscode;
    extensions = with unstable.vscode-extensions; [
      ms-vscode.cpptools
    ];
  };
}
