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

  commit = "8fa84589eef3538dbc763ff98dc7d5a8a0c56374";

  sha256 = {
    x86_64-linux = "16v2v0rv2jrd1rsynx1yhn9093q2px8w8c8lxh0dn63w8dk901yw";
    x86_64-darwin = "1xa9i2fj3ghkslqigf3x148p61l5fw9qs53s02kka4s3rwd0f4wq";
    aarch64-darwin = "087dz88xc5pbsgrhq2172a6hnl4bb9im67qmpdsnac1xi72z38hk";
  }.${system};
  # End of the borrowed nixpkgs code segment from above
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "1.87.0-${commit}";
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
