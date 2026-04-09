{ pkgs, ... }:

let
  # Build BoringSSL from rama-boring-sys source with rama's patches applied
  ramaBoringssl = pkgs.stdenv.mkDerivation rec {
    pname = "rama-boringssl";
    # rama-boring-sys version from codex's Cargo.lock (not the codex git tag)
    version = "0.5.10";

    # Download rama-boring-sys from crates.io which includes the BoringSSL source and patches
    src = pkgs.fetchurl {
      url = "https://crates.io/api/v1/crates/rama-boring-sys/${version}/download";
      sha256 = "sha256-1b/j6G1x6bkdrnVh1c7qzrN6fU/AeKskGv16q3d/YG8=";
    };

    nativeBuildInputs = with pkgs; [
      cmake
      perl
      go
      clang
      llvm
      git
    ];

    # Disable the default cmake configure
    dontUseCmakeConfigure = true;

    unpackPhase = ''
      runHook preUnpack
      mkdir -p source
      cd source
      tar -xzf $src
      runHook postUnpack
    '';

    configurePhase = ''
      runHook preConfigure

      cd rama-boring-sys-${version}/deps/boringssl

      # Apply rama patches using git
      git init
      git config user.email "nix@localhost"
      git config user.name "Nix"
      git add -A
      git commit -m "initial"

      echo "Applying rama_tls.patch..."
      git apply -v --whitespace=fix ../../patches/rama_tls.patch

      echo "Applying rama_boring_pq.patch..."
      git apply -v --whitespace=fix ../../patches/rama_boring_pq.patch

      runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild

      # Build BoringSSL with cmake
      mkdir -p build
      cd build

      cmake .. \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_C_COMPILER=${pkgs.clang}/bin/clang \
        -DCMAKE_CXX_COMPILER=${pkgs.clang}/bin/clang++ \
        -DOPENSSL_NO_ASM=OFF \
        -DBUILD_SHARED_LIBS=OFF

      make -j$NIX_BUILD_CORES crypto ssl

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/lib $out/include $out/build/crypto $out/build/ssl $out/build

      # Copy libraries with the structure rama-boring-sys expects
      # Libraries are built directly in the build directory (not in subdirs)
      cp libcrypto.a $out/build/crypto/
      cp libssl.a $out/build/ssl/
      cp libcrypto.a $out/build/
      cp libssl.a $out/build/

      # Also copy to lib for convenience
      cp libcrypto.a $out/lib/
      cp libssl.a $out/lib/

      # Copy headers from parent directory (boringssl/include)
      cp -r ../include/openssl $out/include/

      runHook postInstall
    '';
  };

  opensslPkg = pkgs.openssl;

  rustPlatform =
    if pkgs.stdenv.isLinux then
      pkgs.makeRustPlatform {
        inherit (pkgs.unstable) cargo rustc;
      }
    else
      pkgs.rustPlatform;

  # Replace the upstream runfiles git dependency with a tiny local stub.
  # rules_rust ships examples that use -Z bindeps, which breaks nixpkgs'
  # cargo vendor utility. codex-utils-cargo-bin is only used by test helpers,
  # and tests are disabled for this package, so the stub is sufficient.
  cargoHashes = {
    x86_64-linux = "sha256-ldBYkXlnC/h5HUhPC/CGwL6S0dxJzQsDEXA2DgJeDRE=";
    aarch64-linux = "sha256-ldBYkXlnC/h5HUhPC/CGwL6S0dxJzQsDEXA2DgJeDRE=";
    x86_64-darwin = "sha256-ldBYkXlnC/h5HUhPC/CGwL6S0dxJzQsDEXA2DgJeDRE=";
    aarch64-darwin = "sha256-ldBYkXlnC/h5HUhPC/CGwL6S0dxJzQsDEXA2DgJeDRE=";
  };

  rustyV8Version = "146.4.0";

  rustyV8Targets = {
    x86_64-linux = "x86_64-unknown-linux-gnu";
    aarch64-linux = "aarch64-unknown-linux-gnu";
    x86_64-darwin = "x86_64-apple-darwin";
    aarch64-darwin = "aarch64-apple-darwin";
  };

  rustyV8ArchiveHashes = {
    x86_64-linux = "sha256-5ktNmeSuKTouhGJEqJuAF4uhA4LBP7WRwfppaPUpEVM=";
    aarch64-linux = "sha256-2/FlsHyBvbBUvARrQ9I+afz3vMGkwbW0d2mDpxBi7Ng=";
    x86_64-darwin = "sha256-YwzSQPG77NsHFBfcGDh6uBz2fFScHFFaC0/Pnrpke7c=";
    aarch64-darwin = "sha256-v+LJvjKlbChUbw+WWCXuaPv2BkBfMQzE4XtEilaM+Yo=";
  };

  rustyV8Archive = pkgs.fetchurl {
    url = "https://github.com/denoland/rusty_v8/releases/download/v${rustyV8Version}/librusty_v8_release_${
      rustyV8Targets.${pkgs.stdenv.system}
    }.a.gz";
    hash = rustyV8ArchiveHashes.${pkgs.stdenv.system};
  };
in
rustPlatform.buildRustPackage (
  rec {
    pname = "codex-cli";
    version = "rust-v0.118.0";

    src = pkgs.fetchFromGitHub {
      owner = "openai";
      repo = "codex";
      rev = "${version}";
      hash = "sha256-FdtV+CIqTInnegcXrXBxw4aE0JnNDh4GdYKwUDjSk9Y=";
    };

    sourceRoot = "source/codex-rs";

    cargoHash = cargoHashes.${pkgs.stdenv.system};

    cargoPatches = [
      ./stub-runfiles.patch
    ];

    postPatch = ''
      mapfile -t vendored_v8_dirs < <(
        while IFS= read -r cargo_toml; do
          dirname "$cargo_toml"
        done < <(
          find "$cargoDepsCopy" -type f -name Cargo.toml \
            -path '*/v8*/Cargo.toml' -print
        ) | sort -u
      )

      if [ "''${#vendored_v8_dirs[@]}" -ne 1 ]; then
        echo "Expected exactly one vendored v8 crate, found ''${#vendored_v8_dirs[@]}" >&2
        printf '%s\n' "''${vendored_v8_dirs[@]}" >&2
        exit 1
      fi

      # Match upstream Codex's rusty_v8 prebuilt archive handling.
      patch -d "''${vendored_v8_dirs[0]}" -p1 < ${./rusty-v8-prebuilt-out-dir.patch}
    ''
    + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
      # LLVM 21 / rustc 1.91.1 currently crashes in release optimization passes
      # for this workspace. Lower Linux release optimization until nixpkgs updates.
      perl -0pi -e 's/\\[profile\\.release\\]\\n/[profile.release]\\nopt-level = 2\\n/' Cargo.toml
    '';

    # Enable unstable features (file_lock)
    RUSTC_BOOTSTRAP = "1";

    # Disable LTO to work around LLVM 21.1.2 + rustc 1.91.1 ICE during LTO codegen.
    # The crash occurs in LLVMContextDispose when building with lto=fat.
    # This affects both binary size and runtime performance (5-20% slower).
    # TODO: Re-enable LTO ("fat" or "thin") once nixpkgs updates LLVM/rustc.
    # To test: remove this line and run `nix build .#codex-cli`
    CARGO_PROFILE_RELEASE_LTO = "off";

    # Upstream pins codegen-units=1 for smaller binaries, but that currently
    # triggers an LLVM 21 / rustc 1.91.1 SIGSEGV while compiling `image` on Linux.
    # Use multiple codegen units until nixpkgs moves past the buggy toolchain.
    CARGO_PROFILE_RELEASE_CODEGEN_UNITS = "16";

    # Show backtrace for build failures
    RUST_BACKTRACE = "1";

    # Keep rusty_v8 fully offline inside the Nix sandbox.
    RUSTY_V8_ARCHIVE = "${rustyV8Archive}";

    # Point rama-boring-sys to our pre-built patched BoringSSL
    BORING_BSSL_PATH = "${ramaBoringssl}";
    BORING_BSSL_INCLUDE_PATH = "${ramaBoringssl}/include";

    nativeBuildInputs =
      with pkgs;
      [
        pkg-config
        cmake
        perl
        go
        clang
        llvm
      ]
      ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
        autoPatchelfHook
        rust-bindgen
        rustPlatform.bindgenHook # for openssl-sys bindgen
      ];

    buildInputs =
      with pkgs;
      pkgs.lib.optionals pkgs.stdenv.isLinux [
        libcap
        opensslPkg # for openssl-sys crate
        stdenv.cc.cc.lib # for libgcc_s.so.1
      ];

    # Ensure C++ standard library is linked for BoringSSL
    NIX_LDFLAGS = pkgs.lib.optionalString pkgs.stdenv.isDarwin "-lc++";

    doCheck = false;

    # Keep Darwin builds unstripped. A full Home Manager switch on macOS produced
    # unusable Codex binaries during fixup, while Linux benefits from normal strip.
    dontStrip = pkgs.stdenv.isDarwin;

    meta = with pkgs.lib; {
      description = "Lightweight coding agent that runs in your terminal";
      homepage = "https://github.com/openai/codex";
      license = licenses.asl20;
      mainProgram = "codex";
    };
  }
  // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
    # Point openssl-sys to system OpenSSL on Linux (avoid BoringSSL detection)
    OPENSSL_DIR = "${opensslPkg.out}";
    OPENSSL_LIB_DIR = "${opensslPkg.out}/lib";
    OPENSSL_INCLUDE_DIR = "${opensslPkg.dev}/include";
    OPENSSL_NO_VENDOR = "1";
  }
)
