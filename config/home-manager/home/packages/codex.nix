{ pkgs, unstable, ... }:

unstable.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.22.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-JTwtydW8LLBH/55+8a/BbqlZtkXsFKbT8dGoDEAjk1c=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-3PljlyPfDsnjGmR/0iM7Fu1TnyDj31pKVcOU/izsL30=";

  nativeBuildInputs = with unstable; [ pkg-config ];
  buildInputs = with unstable; [ openssl ];

  doCheck = false;

  meta = with pkgs.lib; {
    description = "Lightweight coding agent that runs in your terminal";
    homepage = "https://github.com/openai/codex";
    license = licenses.asl20;
    mainProgram = "codex";
  };
}
