{ pkgs, lib, ... }:

let
  pname = "opencode";
  # OpenCode V2 (opencode2). nixpkgs only ships V1, which rejects V2 config.
  version = "2.0.26";

  sources = {
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@opencode/cli-darwin-arm64/-/cli-darwin-arm64-${version}.tgz";
      hash = "sha256-MTDJAkHRxaZEkF2jTxadP3zG3Cc9saM8rJjN1DOBkiA=";
    };
    x86_64-linux = {
      url = "https://registry.npmjs.org/@opencode/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
      hash = "sha256-Kl8A3qCirL60rXL3i8BmzAqMLfFOUmvqyi1gxsZGuTY=";
    };
  };

  sourceInfo =
    sources.${pkgs.stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}");
in
pkgs.stdenv.mkDerivation rec {
  inherit pname version;

  src = pkgs.fetchzip {
    inherit (sourceInfo) url hash;
  };

  nativeBuildInputs = [
    pkgs.makeWrapper
  ]
  ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.autoPatchelfHook ];

  buildInputs = lib.optionals pkgs.stdenv.isLinux [ pkgs.stdenv.cc.cc.lib ];

  dontBuild = true;
  # Bun-compiled single binary; stripping drops the embedded payload.
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/${pname}
    cp -r . $out/lib/${pname}
    chmod +x $out/lib/${pname}/bin/opencode

    # The npm package exposes the same binary as both opencode and opencode2.
    mkdir -p $out/bin
    for name in opencode opencode2; do
      makeWrapper $out/lib/${pname}/bin/opencode $out/bin/$name \
        --set OPENCODE_DISABLE_AUTOUPDATE 1
    done

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    export HOME=$(mktemp -d)
    $out/bin/opencode2 --version | grep -q "${version}"
  '';

  meta = with lib; {
    description = "OpenCode V2 - AI coding agent for the terminal";
    homepage = "https://github.com/anomalyco/opencode";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "opencode2";
    platforms = [
      "x86_64-linux"
      "aarch64-darwin"
    ];
  };
}
