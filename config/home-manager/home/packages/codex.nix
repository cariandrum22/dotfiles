{ pkgs, unstable, ... }:

let
  # Use fenix to get a newer Rust version
  fenix = import (fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz") {
    inherit (pkgs) system;
  };

  # Get Rust nightly which should have all the required features stabilized
  rustToolchain = fenix.latest;

  # Create custom rustPlatform with newer Rust
  customRustPlatform = pkgs.makeRustPlatform {
    inherit (rustToolchain) cargo rustc;
  };
in
# Use custom Rust platform with newer compiler
customRustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.23.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-JS2nRh3/MNQ0mfdr2/Q10sAB38yWBLpw2zFf0dJORuM=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-HbhOiTO7qFp64v+Bb62V1LxPH7qeTnWwkJKPEN4Vx6c=";

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
