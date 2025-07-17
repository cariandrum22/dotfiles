{ pkgs, lib, ... }:

let
  extensions = import ./extensions.nix;
  settings = import ./settings.nix { inherit pkgs; };
  metadata = import ./metadata.nix;

  inherit (pkgs.stdenv.hostPlatform) system;

  # Platform mappings
  platforms = {
    x86_64-linux = {
      vscode-plat = "linux-x64";
      archive = "tar.gz";
    };
    aarch64-darwin = {
      vscode-plat = "darwin-arm64";
      archive = "zip";
    };
  };

  platformInfo = platforms.${system} or (throw "Unsupported system: ${system}");

  inherit (metadata) commit;
  sha256 = metadata.sha256.${system} or (throw "No sha256 for system: ${system}");
in
{
  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    package = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
      pname = "vscode-insiders";
      version = "${metadata.version}-${commit}";
      src = builtins.fetchurl {
        name = "${pname}-${version}.${platformInfo.archive}";
        url = "https://update.code.visualstudio.com/commit:${commit}/insider/${platformInfo.vscode-plat}/stable";
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
