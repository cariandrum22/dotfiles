{ pkgs, unstable, ... }:

unstable.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.65.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-1zh6ifwavP3RFqQbHIClEQ4cKRQ5LdU61LReA6+HSW8=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-XzqEI8wpI5g1+u+AOWiZYOzCVhwysMtHKPDpcfu2HAU=";

  # Enable unstable features (file_lock)
  RUSTC_BOOTSTRAP = "1";

  nativeBuildInputs =
    with unstable;
    [
      pkg-config
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      autoPatchelfHook
    ];
  buildInputs =
    with unstable;
    [
      openssl
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      stdenv.cc.cc.lib # for libgcc_s.so.1
    ];

  doCheck = false;

  meta = with pkgs.lib; {
    description = "Lightweight coding agent that runs in your terminal";
    homepage = "https://github.com/openai/codex";
    license = licenses.asl20;
    mainProgram = "codex";
  };
}
