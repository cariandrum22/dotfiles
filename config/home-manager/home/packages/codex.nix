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

  # Replace the upstream runfiles git dependency with a tiny local stub.
  # rules_rust ships examples that use -Z bindeps, which breaks nixpkgs'
  # cargo vendor utility. codex-utils-cargo-bin is only used by test helpers,
  # and tests are disabled for this package, so the stub is sufficient.
  cargoHashes = {
    x86_64-linux = "sha256-F53k63oSpXoC2FHiEjbBRbtaMzNH82xpU/g86ZfoWuo=";
    aarch64-darwin = "sha256-F53k63oSpXoC2FHiEjbBRbtaMzNH82xpU/g86ZfoWuo=";
  };
in
pkgs.rustPlatform.buildRustPackage (
  rec {
    pname = "codex-cli";
    version = "rust-v0.117.0";

    src = pkgs.fetchFromGitHub {
      owner = "openai";
      repo = "codex";
      rev = "${version}";
      hash = "sha256-Ezd8KaEMVeJPKC74CSPhecPLZghm0v6JkQTCPhr/2nY=";
    };

    sourceRoot = "source/codex-rs";

    cargoHash = cargoHashes.${pkgs.stdenv.system};

    cargoPatches = [
      ./stub-runfiles.patch
    ];

    # Enable unstable features (file_lock)
    RUSTC_BOOTSTRAP = "1";

    # Disable LTO to work around LLVM 21.1.2 + rustc 1.91.1 ICE during LTO codegen.
    # The crash occurs in LLVMContextDispose when building with lto=fat.
    # This affects both binary size and runtime performance (5-20% slower).
    # TODO: Re-enable LTO ("fat" or "thin") once nixpkgs updates LLVM/rustc.
    # To test: remove this line and run `nix build .#codex-cli`
    CARGO_PROFILE_RELEASE_LTO = "off";

    # Show backtrace for build failures
    RUST_BACKTRACE = "1";

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
