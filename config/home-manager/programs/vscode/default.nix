{
  pkgs,
  lib,
  home-manager,
  ...
}:

let
  extensions = import ./extensions.nix;
  settings = import ./settings.nix { inherit pkgs; };
  metadata = import ./metadata.nix;
  mkVscodeModule = import "${home-manager}/modules/programs/vscode/mkVscodeModule.nix";

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

  # Copilot's bundled SDK ships native modules under resources/app/extensions.
  # autoPatchelf needs these as build inputs to resolve NEEDED entries, and as
  # runtime dependencies to keep the patched RPATHs available.
  copilotSdkNativeDependencies = lib.optionals pkgs.stdenv.isLinux [
    pkgs.libei
    pkgs.libjpeg8.out
    pkgs.pipewire
    pkgs.libxtst
  ];

  vscodeInsidersPackage = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs (oldAttrs: rec {
    pname = "vscode-insiders";
    version = "${metadata.version}-${commit}";
    src = builtins.fetchurl {
      name = "${pname}-${version}.${platformInfo.archive}";
      url = metadata.url.${system} or (throw "No URL for system: ${system}");
      inherit sha256;
    };
    buildInputs =
      oldAttrs.buildInputs
      ++ [
        pkgs.krb5
      ]
      ++ copilotSdkNativeDependencies
      ++ lib.optionals pkgs.stdenv.isLinux [
        pkgs.webkitgtk_4_1
        pkgs.libsoup_3
      ];
    runtimeDependencies = lib.optionals pkgs.stdenv.isLinux (
      oldAttrs.runtimeDependencies
      ++ copilotSdkNativeDependencies
      ++ [
        pkgs.libsecret
        pkgs.musl
      ]
    );
    autoPatchelfIgnoreMissingDeps = lib.optionals pkgs.stdenv.isLinux [ "libc.musl-x86_64.so.1" ];
    __impureHostDeps = lib.optionals pkgs.stdenv.isDarwin [
      "/usr/bin/codesign"
      "/usr/libexec/PlistBuddy"
    ];
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
    postPatch =
      let
        productJson =
          if pkgs.stdenv.isDarwin then
            "Contents/Resources/app/product.json"
          else
            "resources/app/product.json";
      in
      ''
        productJson="${productJson}"
        proposalPath='.extensionEnabledApiProposals["ms-vscode-remote.remote-ssh"]'

        if ! ${lib.getExe pkgs.jq} -e "$proposalPath | type == \"array\"" "$productJson" >/dev/null; then
          echo "Remote SSH API proposal allowlist is missing from $productJson" >&2
          exit 1
        fi

        if ! ${lib.getExe pkgs.jq} -e \
          "$proposalPath | index(\"terminalRemoteResolver\") != null" \
          "$productJson" >/dev/null; then
          tmpProductJson="$(mktemp)"
          ${lib.getExe pkgs.jq} \
            "$proposalPath += [\"terminalRemoteResolver\"]" \
            "$productJson" > "$tmpProductJson"
          mv "$tmpProductJson" "$productJson"
        fi
      ''
      + lib.optionalString pkgs.stdenv.isLinux ''
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
    postInstall = lib.optionalString pkgs.stdenv.isDarwin ''
      app="$out/Applications/Visual Studio Code - Insiders.app"

      # product.json changes invalidate the vendor signature, so re-sign all code.
      /usr/bin/codesign --deep --force --sign - \
        --preserve-metadata=identifier,entitlements,flags,runtime \
        "$app"

      # Ad-hoc signatures have no shared Team ID. Allow each Electron app bundle
      # to load the re-signed framework, processing children before their parent.
      while IFS= read -r -d "" bundle; do
        entitlements="$(mktemp)"
        /usr/bin/codesign -d --entitlements :- "$bundle" > "$entitlements" 2>/dev/null

        if ! /usr/libexec/PlistBuddy \
          -c 'Print :com.apple.security.cs.disable-library-validation' \
          "$entitlements" >/dev/null 2>&1; then
          /usr/libexec/PlistBuddy \
            -c 'Add :com.apple.security.cs.disable-library-validation bool true' \
            "$entitlements"
        fi

        /usr/bin/codesign --force --sign - \
          --preserve-metadata=identifier,flags,runtime \
          --entitlements "$entitlements" \
          "$bundle"
        rm "$entitlements"
      done < <(find "$app" -depth -type d -name "*.app" -print0)
    '';
  });
in
{
  imports = [
    (mkVscodeModule {
      modulePath = [
        "programs"
        "vscodeInsiders"
      ];
      name = "Visual Studio Code - Insiders";
      packageName = "vscode";
      nameShort = "Code - Insiders";
      dataFolderName = ".vscode-insiders";
    })
  ];

  config = {
    programs.vscodeInsiders = {
      enable = true;
      mutableExtensionsDir = false;
      package = vscodeInsidersPackage;
      profiles.default = {
        extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace extensions;
        inherit (settings) userSettings;
      };
    };

    home.activation.cleanupEmptyVscodeInsidersExtensions =
      lib.hm.dag.entryBefore [ "checkLinkTargets" ]
        ''
          target="$HOME/.vscode-insiders/extensions"

          if [[ -d "$target" && ! -L "$target" ]]; then
            mapfile -t entries < <(find "$target" -mindepth 1 -maxdepth 1 -print)

            if [[ ''${#entries[@]} -eq 0 ]]; then
              run rmdir "$target"
            elif [[ ''${#entries[@]} -eq 1 && "''${entries[0]}" == "$target/extensions.json" ]]; then
              extensions_json="$(tr -d '[:space:]' < "$target/extensions.json" 2>/dev/null || true)"

              if [[ "$extensions_json" == "[]" ]]; then
                run rm "$target/extensions.json"
                run rmdir "$target"
              fi
            fi
          fi
        '';
  };
}
