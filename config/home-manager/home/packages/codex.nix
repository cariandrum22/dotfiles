{ pkgs, unstable, ... }:

let
  # Use fenix to get a newer Rust version
  fenix = import (fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz") {
    inherit (pkgs) system;
  };

  # Get Rust nightly which should have all the required features stabilized
  rustToolchain = fenix.latest;

  # Create custom rustPlatform with newer Rust
  customRustPlatform = unstable.makeRustPlatform {
    inherit (rustToolchain) cargo rustc;
  };
in
# Use custom Rust platform with newer compiler
customRustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.34.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-C1PXK/5vPFV5cz1dYWV+GaYl0grscb6qCR66BSih5/E=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-qJn2oN/9LVLhHnaNp+x9cUEMODrGrgV3SiR0ykIx7B4=";

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
