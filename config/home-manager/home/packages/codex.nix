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
  version = "rust-v0.36.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-tqHJx35Y5gxdyjqWV+hCgdHokSINTyUP7pq7GEsnfTk=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-/2Qk9sq3id8rKVl2fvIZeI/WhfPhiMTvvnIW+bW9+BU=";

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