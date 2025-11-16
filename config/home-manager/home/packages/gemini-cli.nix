{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "gemini-cli";
  version = "0.17.0-nightly.20251116.e650a4ee5";

  src = pkgs.fetchurl {
    url = "https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js";
    hash = "sha256-8ATRm6bl7TVt1szO+L4ZeUH8OreWniV0WJ5r3jTEuTE=";
  };

  dontUnpack = true;

  nativeBuildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib
    cp $src $out/lib/gemini.js
    chmod +x $out/lib/gemini.js

    mkdir -p $out/bin
    makeWrapper ${pkgs.nodejs_22}/bin/node $out/bin/gemini \
      --add-flags "$out/lib/gemini.js"

    runHook postInstall
  '';

  meta = with lib; {
    description = "An open-source AI agent that brings the power of Gemini directly into your terminal";
    homepage = "https://github.com/google-gemini/gemini-cli";
    license = licenses.asl20;
    mainProgram = "gemini";
  };
}
