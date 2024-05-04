{ lib, pkgs, ... }:

let
  extensions = import ./extensions.nix;
  settings = import ./settings.nix { inherit pkgs; };

  # Start of the code segment borrowed from nixpkgs
  # (https://github.com/NixOS/nixpkgs/blob/nixos-23.05/pkgs/applications/editors/vscode/vscode.nix)
  # The original code is licensed under the MIT license.
  inherit (pkgs.stdenv.hostPlatform) system;

  plat = {
    x86_64-linux = "linux-x64";
    x86_64-darwin = "darwin";
    aarch64-darwin = "darwin-arm64";
  }.${system};

  archive_fmt = if pkgs.stdenv.isDarwin then "zip" else "tar.gz";

  commit = "816c6f7d7bc4da9796cd19ac6b122b6e26f50d7f";

  sha256 = {
    x86_64-linux = "1xcy8rh3ij2r2ra04kyag2a8ih6rklyl5v2yyqpq4gnl84q8h5nm";
    x86_64-darwin = "152y04wj7aw0ajszh425sq40xyazi012rklncg3jvkbzvlac03pc";
    aarch64-darwin = "1h5a905qd93rx18fxpw2d6dr78glcrcjb4zc1hylfpqz42xcvd8m";
  }.${system};
  # End of the borrowed nixpkgs code segment from above
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "1.90.0-${commit}";
      src = (builtins.fetchurl {
        name = "${pname}-${version}.${archive_fmt}";
        url = "https://code.visualstudio.com/sha/download?build=insider&os=${plat}";
        inherit sha256;
      });
      buildInputs = oldAttrs.buildInputs ++ [ pkgs.krb5 ];
      runtimeDependencies = lib.optionals pkgs.stdenv.isLinux (oldAttrs.runtimeDependencies ++ [ pkgs.libsecret ]);
      urlHandlerDesktopItem = pkgs.makeDesktopItem {
        name = "code-insiders-url-handler";
        desktopName = "Visual Studio Code - Insiders - URL Handler";
        comment = "Code Editing. Redefined.";
        genericName = "Text Editor";
        exec = "code-insiders" + " --open-url %U";
        icon = "code";
        startupNotify = true;
        categories = [ "Utility" "TextEditor" "Development" "IDE" ];
        mimeTypes = [ "x-scheme-handler/${pname}" ];
        keywords = [ "vscode" ];
        noDisplay = true;
      };
    });
    extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace extensions;
  } // settings;
}
