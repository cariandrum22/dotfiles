{ pkgs, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.72.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-rNol7k/CcAKJXZYsbORRqD+uJfN6TPfcEbkUXezpFkY=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-ZTrG3m6fEwn5ifNFi1A57/sdUfIaEUXMjmoQ86kAxGM=";

  # Enable unstable features (file_lock)
  RUSTC_BOOTSTRAP = "1";

  # Disable LTO to work around LLVM 21.1.2 + rustc 1.91.1 ICE during LTO codegen.
  # The crash occurs in LLVMContextDispose when building with lto=fat.
  # This affects both binary size and runtime performance (5-20% slower).
  # TODO: Re-enable LTO ("fat" or "thin") once nixpkgs updates LLVM/rustc.
  # To test: remove this line and run `nix build .#codex-cli`
  CARGO_PROFILE_RELEASE_LTO = "off";

  nativeBuildInputs =
    with pkgs;
    [
      pkg-config
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      autoPatchelfHook
    ];
  buildInputs =
    with pkgs;
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
