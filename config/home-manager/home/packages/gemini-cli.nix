{ pkgs, lib, ... }:

let
  pname = "gemini-cli";
  version = "0.36.0";
  assetName = "gemini-cli-bundle.zip";
  isArchive = lib.hasSuffix ".zip" assetName;
in
pkgs.stdenv.mkDerivation rec {
  inherit pname version;

  src = (if isArchive then pkgs.fetchzip else pkgs.fetchurl) (
    {
      url = "https://github.com/google-gemini/gemini-cli/releases/download/v${version}/${assetName}";
      hash = "sha256-wu+QZ5roBNY1mwtte+7opKFBRdOCXONW95UEJ7M3gJI=";
    }
    // lib.optionalAttrs isArchive {
      stripRoot = false;
    }
  );

  dontUnpack = !isArchive;
  dontBuild = true;

  nativeBuildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/gemini-cli
  ''
  + (
    if isArchive then
      ''
        cp -r . $out/lib/gemini-cli
      ''
    else
      ''
        cp $src $out/lib/gemini-cli/gemini.js
      ''
  )
  + ''
    chmod +x $out/lib/gemini-cli/gemini.js

    mkdir -p $out/bin
    makeWrapper ${pkgs.nodejs_22}/bin/node $out/bin/gemini \
      --add-flags "$out/lib/gemini-cli/gemini.js"

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    export HOME=$(mktemp -d)
    $out/bin/gemini --version | grep -q "${version}"
  '';

  meta = with lib; {
    description = "An open-source AI agent that brings the power of Gemini directly into your terminal";
    homepage = "https://github.com/google-gemini/gemini-cli";
    license = licenses.asl20;
    mainProgram = "gemini";
  };
}
