{ pkgs, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.7.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-EPK7FX7mP6+nhYMs22zjAclZ9067lOo+BoBYckdnq/E=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-69bvdxHdFnWYvCH0PW+KMZI5HDuarFndNlf75Iw5Dno=";

  nativeBuildInputs = with pkgs; [ pkg-config ];
  buildInputs = with pkgs; [ openssl ];

  doCheck = false;

  meta = with pkgs.lib; {
    description = "Lightweight coding agent that runs in your terminal";
    homepage = "https://github.com/openai/codex";
    license = licenses.asl20;
    mainProgram = "codex";
  };
}
