{ pkgs, lib, ... }:

let
  pname = "droid";
  version = "0.226.2";

  # Platform-specific source URLs and hashes
  sources = {
    aarch64-darwin = {
      url = "https://downloads.factory.ai/factory-cli/releases/${version}/darwin/arm64/droid";
      hash = "sha256-y5KcwhlXyyz9Rf4vfqxn3BjJURm1om5C77wOohZIsUE=";
    };
    x86_64-linux = {
      url = "https://downloads.factory.ai/factory-cli/releases/${version}/linux/x64/droid";
      hash = "sha256-myqaj79AaVSOurU1D2Ba+YouAAiFVmWdp7QriT0jogE=";
    };
  };

  # Select the appropriate source for the current system
  sourceInfo =
    sources.${pkgs.stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}");
in
pkgs.stdenv.mkDerivation rec {
  inherit pname version;

  src = pkgs.fetchurl { inherit (sourceInfo) url hash; };

  dontUnpack = true;

  nativeBuildInputs = lib.optionals pkgs.stdenv.isLinux [
    pkgs.autoPatchelfHook
  ];

  buildInputs = lib.optionals pkgs.stdenv.isLinux [
    pkgs.stdenv.cc.cc.lib # for libgcc_s.so.1
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp $src $out/bin/droid
    chmod +x $out/bin/droid

    runHook postInstall
  '';

  # Droid CLI requires ripgrep
  propagatedBuildInputs = [ pkgs.ripgrep ];

  meta = with lib; {
    description = "Factory AI Droid CLI - AI agent for software development";
    homepage = "https://factory.ai";
    license = licenses.unfree;
    mainProgram = "droid";
    platforms = [
      "x86_64-linux"
      "aarch64-darwin"
    ];
  };
}
