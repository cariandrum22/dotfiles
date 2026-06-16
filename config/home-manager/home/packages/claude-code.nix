{ pkgs, lib, ... }:

let
  pname = "claude-code";
  version = "2.1.178";

  sources = {
    aarch64-darwin = {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code-darwin-arm64/-/claude-code-darwin-arm64-${version}.tgz";
      hash = "sha256-CSU8S3Js3T+XfzZ/O9ea9g2ihYbzinGi95PxZ1rGMnY=";
    };
    x86_64-linux = {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/claude-code-linux-x64-${version}.tgz";
      hash = "sha256-qPICZNamwShVJj3ABHmQtqAbrLxKN0U8SrKzeoDbIDE=";
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
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/${pname}
    cp -r . $out/lib/${pname}
    chmod +x $out/lib/${pname}/claude

    mkdir -p $out/bin
    makeWrapper $out/lib/${pname}/claude $out/bin/claude \
      --set CLAUDE_NO_AUTO_UPDATE 1

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    export HOME=$(mktemp -d)
    $out/bin/claude --version | grep -q "${version}"
  '';

  meta = with lib; {
    description = "Use Claude, Anthropic's AI assistant, right from your terminal";
    homepage = "https://github.com/anthropics/claude-code";
    license = licenses.unfree;
    maintainers = [ ];
    mainProgram = "claude";
    platforms = [
      "x86_64-linux"
      "aarch64-darwin"
    ];
  };
}
