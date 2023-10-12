{ lib, pkgs, ... }:

let
  extensions = import ./extensions.nix;
  settings = import ./settings.nix { inherit pkgs; };

  extensions_from_nixpkgs = with builtins; let
    es = map (e: lib.splitString "." e)
      (filter (i: i != "")
        (lib.splitString "\n" (readFile ./extensions-from-nixpkgs)));
  in
  map (e: pkgs.vscode-extensions.${elemAt e 0}.${elemAt e 1}) es;

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

  commit = "501baeb5e07b5a404f862d922bacb93771d91331";

  sha256 = {
    x86_64-linux = "1prshql24lbwgydbsv115vqqicgziixd3nllzc891a1f4f74vy53";
    x86_64-darwin = "0rlcilz27zhwv3xjgrl03yzgwa4ky97c7ipi16mjh7xvms2vdshj";
    aarch64-darwin = "0xfh4i80n0vn6s4blkxqdpg32nlwl8pdlmz45ymnswn0x1lj8ysq";
  }.${system};
  # End of the borrowed nixpkgs code segment from above
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "1.82.0-${commit}";
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
    extensions = extensions_from_nixpkgs
      ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace extensions;
  } // settings;
}
