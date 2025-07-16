{ pkgs, lib, ... }:

let
  extensions = import ./extensions.nix;
  settings = import ./settings.nix { inherit pkgs; };

  # Start of the code segment borrowed from nixpkgs
  # (https://github.com/NixOS/nixpkgs/blob/nixos-23.05/pkgs/applications/editors/vscode/vscode.nix)
  # The original code is licensed under the MIT license.
  inherit (pkgs.stdenv.hostPlatform) system;

  plat =
    {
      x86_64-linux = "linux-x64";
      aarch64-darwin = "darwin-arm64";
    }
    .${system};

  archive_fmt = if pkgs.stdenv.isDarwin then "zip" else "tar.gz";
  commit = "b5d2dfbd1133331d3ff6f9fca1a4d8920d5cbeb9";

  sha256 =
    {
      x86_64-linux = "080a74n6np754kdv92vcygvbs7ywbgkpbw5sl8fqqm8l4n8n3zqs";
      aarch64-darwin = "16qfsl6nffr6ipc2g36cp9drd3d3jmpmvnvk353yvdaq4f7hsvp5";
    }
    .${system};
in
# End of the borrowed nixpkgs code segment from above
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "1.103.0-${commit}";
      src = builtins.fetchurl {
        name = "${pname}-${version}.${archive_fmt}";
        url = "https://code.visualstudio.com/sha/download?build=insider&os=${plat}";
        inherit sha256;
      };
      buildInputs = oldAttrs.buildInputs ++ [ pkgs.krb5 ];
      runtimeDependencies = lib.optionals pkgs.stdenv.isLinux (
        oldAttrs.runtimeDependencies ++ [ pkgs.libsecret ]
      );
      urlHandlerDesktopItem = pkgs.makeDesktopItem {
        name = "code-insiders-url-handler";
        desktopName = "Visual Studio Code - Insiders - URL Handler";
        comment = "Code Editing. Redefined.";
        genericName = "Text Editor";
        exec = "code-insiders" + " --open-url %U";
        icon = "code";
        startupNotify = true;
        categories = [
          "Utility"
          "TextEditor"
          "Development"
          "IDE"
        ];
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
    profiles.default = {
      extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace extensions;
      inherit (settings) userSettings;
    };
  };
}
