{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "gemini-cli";
  version = "0.5.0-nightly.20250908.4693137b";

  src = pkgs.fetchurl {
    url = "https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js";
    hash = "sha256-ykakLnIA0fR+rCdFOwLDsWPtZy6WYq5ntB5GqZRzrt4=";
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
