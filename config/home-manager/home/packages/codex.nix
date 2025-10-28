{ pkgs, unstable, ... }:

unstable.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.50.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-8qNQ92VV0aog3USzeAMqWXws7kaQ//6/A/M85USTTXY=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-T6Zt5U2aCJWflwKzTbJXwK+BeE7L6IP4WAmISitrpRg=";

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
