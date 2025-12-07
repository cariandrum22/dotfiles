{ pkgs, ... }:
let
  pname = "cursor";
  version = "2.1.50";

  # Fixed download URL - update this with update-cursor.py script
  downloadUrl = "https://downloads.cursor.com/production/56f0a83df8e9eb48585fcc4858a9440db4cc7771/linux/x64/Cursor-2.1.50-x86_64.AppImage";

  src = pkgs.fetchurl {
    url = downloadUrl;
    hash = "sha256-gitfWzovz0OxwoLXIMQ4PqMk+S8KzZlqcp0hmwVR2nE=";
  };
  appimageContents = pkgs.appimageTools.extract { inherit pname version src; };
in
with pkgs;
appimageTools.wrapType2 {
  inherit pname version src;
  extraInstallCommands = ''
    install -m 444 -D ${appimageContents}/${pname}.desktop -t $out/share/applications
    substituteInPlace $out/share/applications/${pname}.desktop \
      --replace-quiet 'Exec=AppRun' 'Exec=${pname}'
    cp -r ${appimageContents}/usr/share/icons $out/share

    # Ensure the binary exists and create a symlink if it doesn't already exist
    if [ -e ${appimageContents}/AppRun ]; then
      install -m 755 -D ${appimageContents}/AppRun $out/bin/${pname}-${version}
      if [ ! -L $out/bin/${pname} ]; then
        ln -s $out/bin/${pname}-${version} $out/bin/${pname}
      fi
    else
      echo "Error: Binary not found in extracted AppImage contents."
      exit 1
    fi
  '';

  extraBwrapArgs = [ "--bind-try /etc/nixos/ /etc/nixos/" ];

  dieWithParent = false;

  extraPkgs = _pkgs: [
    unzip
    autoPatchelfHook
    asar
    # override doesn't preserve splicing https://github.com/NixOS/nixpkgs/issues/132651
    (buildPackages.wrapGAppsHook.override { inherit (buildPackages) makeWrapper; })
  ];
}
