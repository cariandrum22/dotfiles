{ pkgs, unstable, ... }:

let
  # Use fenix to get a newer Rust version with file_lock stabilized
  fenix = import (fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz") {
    inherit (pkgs) system;
  };

  # Get nightly Rust which has unstable features including file_lock
  # file_lock is still unstable as of Rust 1.82 - see https://github.com/rust-lang/rust/issues/130994
  rustToolchain = fenix.latest;

  # Create custom rustPlatform with newer Rust
  customRustPlatform = unstable.makeRustPlatform {
    inherit (rustToolchain) cargo rustc;
  };
in
customRustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.41.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-Dz+RE3Ejr7BcJBJq5+UMP2Pb6v8A2intn3LzozoWovE=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-0sCmo3/3kY+nCufATBySAif5Z/T89Le0UedVgCrZiW8=";

  # Set nightly Rust channel for unstable features
  CARGO_BUILD_RUSTC = "${rustToolchain.rustc}/bin/rustc";
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
