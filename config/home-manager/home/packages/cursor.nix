{ pkgs, ... }:
let
  pname = "cursor";
  version = "3.9.16";

  # Fixed download URL - update this with update-cursor.py script
  downloadUrl = "https://downloads.cursor.com/production/042b3c1a4c53f2c3808067f519fbfc67b72cad8b/linux/x64/Cursor-3.9.16-x86_64.AppImage";

  src = pkgs.fetchurl {
    url = downloadUrl;
    hash = "sha256-dG61VYGMHPip57ldzNICEi1yPc4s1dON+MlDGiKadKc=";
  };
  appimageContents = pkgs.appimageTools.extract { inherit pname version src; };
in
with pkgs;
appimageTools.wrapType2 {
  inherit pname version src;
  extraInstallCommands = ''
    desktop_file=""
    for candidate in \
      ${appimageContents}/${pname}.desktop \
      ${appimageContents}/Cursor.desktop; do
      if [ -f "$candidate" ]; then
        desktop_file="$candidate"
        break
      fi
    done
    if [ -z "$desktop_file" ]; then
      desktop_file="$(find ${appimageContents} -maxdepth 1 -type f -name '*.desktop' | sort | head -n 1)"
    fi
    if [ -z "$desktop_file" ]; then
      echo "Error: Desktop file not found in extracted AppImage contents." >&2
      find ${appimageContents} -maxdepth 2 -type f -print >&2 || true
      exit 1
    fi
    install -m 444 -D "$desktop_file" $out/share/applications/${pname}.desktop
    substituteInPlace $out/share/applications/${pname}.desktop \
      --replace-quiet 'Exec=AppRun' 'Exec=${pname}'
    if [ -d ${appimageContents}/usr/share/icons ]; then
      cp -r ${appimageContents}/usr/share/icons $out/share
    fi

    # Ensure the binary exists and create a symlink if it doesn't already exist.
    if [ -e ${appimageContents}/AppRun ]; then
      install -m 755 -D ${appimageContents}/AppRun $out/bin/${pname}-${version}
    elif [ -e ${appimageContents}/usr/bin/${pname} ]; then
      install -m 755 -D ${appimageContents}/usr/bin/${pname} $out/bin/${pname}-${version}
    elif [ -e ${appimageContents}/usr/bin/Cursor ]; then
      install -m 755 -D ${appimageContents}/usr/bin/Cursor $out/bin/${pname}-${version}
    elif [ -e ${appimageContents}/usr/share/${pname}/${pname} ]; then
      install -m 755 -D ${appimageContents}/usr/share/${pname}/${pname} $out/bin/${pname}-${version}
    else
      echo "Error: Binary not found in extracted AppImage contents." >&2
      find ${appimageContents} -maxdepth 3 -type f -perm -0100 -print >&2 || true
      exit 1
    fi

    if [ ! -L $out/bin/${pname} ]; then
      ln -s $out/bin/${pname}-${version} $out/bin/${pname}
    fi
  '';

  extraBwrapArgs = [ "--bind-try /etc/nixos/ /etc/nixos/" ];

  dieWithParent = false;

  extraPkgs = _pkgs: [
    unzip
    autoPatchelfHook
    asar
    # override doesn't preserve splicing https://github.com/NixOS/nixpkgs/issues/132651
    (buildPackages.wrapGAppsHook3.override { inherit (buildPackages) makeWrapper; })
  ];
}
