# This code is based on https://gist.github.com/lukalot/fcbf3216ad13b8303ab0947af0d5abd5
{ pkgs, lib, ... }:
let
  apiResponse = pkgs.fetchurl {
    url = "https://www.cursor.com/api/download?platform=linux-x64&releaseTrack=stable";
    sha256 = "0vq7rqlncw5mv4dfnrxndcsj16dnwkz7f27i94x1yyf67730zk23";
  };

  data = builtins.fromJSON (builtins.readFile apiResponse);
  inherit (data) downloadUrl;

  pname = "cursor";
  version = "1.2.4";

  src = pkgs.fetchurl {
    url = downloadUrl;
    hash = "sha256-5H7PynQYsZN218rdIdNCor9QGkk4GGyLeGGzGwz436o=";
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

  extraPkgs = pkgs: [
    unzip
    autoPatchelfHook
    asar
    # override doesn't preserve splicing https://github.com/NixOS/nixpkgs/issues/132651
    (buildPackages.wrapGAppsHook.override { inherit (buildPackages) makeWrapper; })
  ];
}
