{ pkgs, ... }:
let
  pname = "cursor";
  version = "1.7.52";

  # Fixed download URL - update this with update-cursor.py script
  downloadUrl = "https://downloads.cursor.com/production/9675251a06b1314d50ff34b0cbe5109b78f848cd/linux/x64/Cursor-1.7.52-x86_64.AppImage";

  src = pkgs.fetchurl {
    url = downloadUrl;
    hash = "sha256-nhDDdXE5/m9uASiQUJ4GHfApkzkf9ju5b8s0h6BhpjQ=";
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
