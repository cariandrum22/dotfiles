{ pkgs, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.73.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-6oVSscGpMcJGOJ90JuwmYe6HbFteoTdTPr2AGU84JzQ=";
  };

  sourceRoot = "source/codex-rs";
  cargoHash = "sha256-MN977yTfdESey0CK8vOXMKjY9HSXqRy1LgK7IYjNz1k=";

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
