{ pkgs, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "claude-code";
  version = "2.0.42";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
    hash = "sha256-Xn1h9Phw4FLrF0EfrY5MLA0RnOuA6Dk+PWqP7fN1DUU=";
  };

  nativeBuildInputs = [ pkgs.makeWrapper ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/node_modules/@anthropic-ai/claude-code
    cp -r . $out/lib/node_modules/@anthropic-ai/claude-code

    mkdir -p $out/bin
    makeWrapper ${pkgs.nodejs_22}/bin/node $out/bin/claude \
      --add-flags "$out/lib/node_modules/@anthropic-ai/claude-code/cli.js" \
      --set CLAUDE_NO_AUTO_UPDATE 1

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    export HOME=$(mktemp -d)
    $out/bin/claude --version | grep -q "${version}"
  '';

  meta = with pkgs.lib; {
    description = "Use Claude, Anthropic's AI assistant, right from your terminal";
    homepage = "https://github.com/anthropics/claude-code";
    license = licenses.unfree;
    maintainers = [ ];
    mainProgram = "claude";
  };
}
