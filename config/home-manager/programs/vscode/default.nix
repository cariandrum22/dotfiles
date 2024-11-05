{ pkgs, lib, ... }:

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

  commit = "19fabc20e35c89915c772116503a079554166a3f";

  sha256 = {
    x86_64-linux = "1c9c99vz7sj1z18ij9cpxppxzbps733ajnlhrabdlm9si30svpi7";
    x86_64-darwin = "0fdkld449l6y1k2s8knvqvs9n6kjvscbm8vzfwwdl60syq20adkq";
    aarch64-darwin = "00wi08ck7n6zrwk584mypjwibp8ds072bj436y93rzs62wf9psmj";
  }.${system};
  # End of the borrowed nixpkgs code segment from above
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "1.96.0-${commit}";
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
      postPatch = lib.optionalString pkgs.stdenv.isLinux ''
        # this is a fix for "save as root" functionality
        packed="resources/app/node_modules.asar"
        unpacked="resources/app/node_modules"
        asar extract "$packed" "$unpacked"
        substituteInPlace $unpacked/@vscode/sudo-prompt/index.js \
          --replace "/usr/bin/pkexec" "/run/wrappers/bin/pkexec" \
          --replace "/bin/bash" "${pkgs.bash}/bin/bash"
        rm -rf "$packed"

        # without this symlink loading JsChardet, the library that is used for auto encoding detection when files.autoGuessEncoding is true,
        # fails to load with: electron/js2c/renderer_init: Error: Cannot find module 'jschardet'
        # and the window immediately closes which renders VSCode unusable
        # see https://github.com/NixOS/nixpkgs/issues/152939 for full log
        ln -rs "$unpacked" "$packed"
      '';
    });
    extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace extensions;
  } // settings;
}
