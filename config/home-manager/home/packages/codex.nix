{ pkgs, unstable, ... }:

unstable.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.44.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-i7UhSQ16HqcX/QUb6JQgF6WOTAEDZZNkL/MSqefcwBI=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-sR7Y1SfP0akLRRvP/6tu3gZRNvQbG/a+4bOSEbbrV30=";

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
