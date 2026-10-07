{ pkgs, lib, ... }:

let
  pname = "antigravity-cli";
  version = "1.3.1";

  # Release tarballs contain a single `antigravity` binary. Linux uses the
  # statically linked musl build so no ELF patching is needed.
  sources = {
    aarch64-darwin = {
      url = "https://github.com/google-antigravity/antigravity-cli/releases/download/${version}/agy_cli_mac_arm64.tar.gz";
      hash = "sha256-7144WzKv2kzxYSNou0vxVdP49MVdUUiGSfUIuu/nfIY=";
    };
    x86_64-linux = {
      url = "https://github.com/google-antigravity/antigravity-cli/releases/download/${version}/agy_cli_linux_x64_musl.tar.gz";
      hash = "sha256-7j4vN4D/p+hSxbh+QtDAFYxsDYPaz0oW7VqilRcMIc8=";
    };
  };

  sourceInfo =
    sources.${pkgs.stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}");
in
pkgs.stdenv.mkDerivation {
  inherit pname version;

  src = pkgs.fetchurl {
    inherit (sourceInfo) url hash;
  };

  sourceRoot = ".";

  nativeBuildInputs = [ pkgs.makeWrapper ];

  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 antigravity $out/lib/${pname}/antigravity

    # The upstream installer exposes the binary as `agy`. The Nix store is
    # read-only, so keep the CLI from attempting self-updates.
    mkdir -p $out/bin
    makeWrapper $out/lib/${pname}/antigravity $out/bin/agy \
      --set AGY_CLI_DISABLE_AUTO_UPDATE 1

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    export HOME=$(mktemp -d)
    $out/bin/agy --version | grep -q "${version}"
  '';

  meta = with lib; {
    description = "Google's Antigravity agent harness in the terminal (successor to Gemini CLI)";
    homepage = "https://github.com/google-antigravity/antigravity-cli";
    license = licenses.unfree;
    maintainers = [ ];
    mainProgram = "agy";
    platforms = [
      "x86_64-linux"
      "aarch64-darwin"
    ];
  };
}
