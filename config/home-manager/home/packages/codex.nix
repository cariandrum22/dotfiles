{ pkgs, ... }:

let
  # Build BoringSSL from rama-boring-sys source with rama's patches applied
  ramaBoringssl = pkgs.stdenv.mkDerivation rec {
    pname = "rama-boringssl";
    version = "0.5.9";

    # Download rama-boring-sys from crates.io which includes the BoringSSL source and patches
    src = pkgs.fetchurl {
      url = "https://crates.io/api/v1/crates/rama-boring-sys/${version}/download";
      sha256 = "sha256-Qh67QERKbXQPhnpQVacQpz5810ubOJWDtF6bKdEzBGU=";
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
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = "codex-cli";
  version = "rust-v0.94.0";

  src = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "${version}";
    hash = "sha256-E8pQbYnXHOc9sy9bh7Icbwxn3Vz5d0nZvmlwM8RV8BU=";
  };

  sourceRoot = "source/codex-rs";

  # Remove utils/cargo-bin from workspace to avoid rules_rust git dependency.
  # The utils/cargo-bin crate depends on 'runfiles' which comes from
  # dzbarsky/rules_rust. That repo contains examples using -Z bindeps,
  # which causes nixpkgs 25.11's cargo vendor utility to fail when parsing.
  cargoPatches = [ ./remove-cargo-bin.patch ];

  cargoHash = "sha256-tV6ubCVQJKOo/7AUfwfpgy4nlUPZ0B6U3Vq28IUbUf4=";

  # Remove ALL codex_utils_cargo_bin references from source files.
  # Tests are disabled (doCheck = false) so stub implementations work fine.
  postPatch =
    let
      # Perl script to replace all codex_utils_cargo_bin references
      perlScript = pkgs.writeText "fix-cargo-bin.pl" ''
        # Comment out use statements first
        s/^(\s*use codex_utils_cargo_bin.*)$/\/\/ REMOVED: $1/g;
        # Replace cargo_bin() calls with ? operator - direct PathBuf
        s/codex_utils_cargo_bin::cargo_bin\("[^"]+"\)\?/std::path::PathBuf::from("stub-binary")/g;
        # Replace cargo_bin().expect() on same line - direct PathBuf
        s/codex_utils_cargo_bin::cargo_bin\("[^"]+"\)\.expect\([^)]+\)/std::path::PathBuf::from("stub-binary")/g;
        # Replace cargo_bin().context()? - direct PathBuf
        s/codex_utils_cargo_bin::cargo_bin\("[^"]+"\)\.context\([^)]+\)\?/std::path::PathBuf::from("stub-binary")/g;
        # Replace bare cargo_bin() - use direct PathBuf (not Ok-wrapped to avoid type inference issues)
        s/codex_utils_cargo_bin::cargo_bin\("[^"]+"\)/std::path::PathBuf::from("stub-binary")/g;
        # Replace find_resource! macro calls - use direct PathBuf
        s/codex_utils_cargo_bin::find_resource!\([^)]+\)\?/std::path::PathBuf::from("stub-resource")/g;
        s/codex_utils_cargo_bin::find_resource!\([^)]+\)/std::path::PathBuf::from("stub-resource")/g;
        s/find_resource!\([^)]+\)\?/std::path::PathBuf::from("stub-resource")/g;
        s/find_resource!\([^)]+\)/std::path::PathBuf::from("stub-resource")/g;
      '';
    in
    ''
      # Apply global replacements to all Rust files
      find . -name "*.rs" -exec perl -i -p ${perlScript} {} \;

      # Fix CargoBinError type reference (the use statement is now commented out)
      substituteInPlace core/tests/common/lib.rs \
        --replace-fail 'Result<String, CargoBinError>' 'Result<String, std::io::Error>'

      # Fix stdio_server_bin - replace the entire function body with a proper stub
      # Original: cargo_bin("test_stdio_server").map(|p| p.to_string_lossy().to_string())
      # After perl: PathBuf::from("stub-binary").map(...) - but PathBuf has no map method
      substituteInPlace core/tests/common/lib.rs \
        --replace-fail 'std::path::PathBuf::from("stub-binary").map(|p| p.to_string_lossy().to_string())' \
        'Err(std::io::Error::new(std::io::ErrorKind::NotFound, "test binary discovery disabled"))'

      # Fix test_codex.rs - remove the if let Ok block entirely (tests disabled)
      sed -i '/if let Ok(path) = std::path::PathBuf::from/,/}/d' core/tests/common/test_codex.rs

      # Fix test_codex_exec.rs - remove .expect() on next line after PathBuf
      sed -i 'N; s/std::path::PathBuf::from("stub-binary")\n[[:space:]]*\.expect([^)]*),/std::path::PathBuf::from("stub-binary"),/g; P; D' core/tests/common/test_codex_exec.rs

      # Fix lib.rs - replace match on PathBuf with direct assignment
      sed -i 's/let full_path = match std::path::PathBuf::from("stub-resource") {/let full_path = std::path::PathBuf::from("stub-resource"); \/\/ stub/g' core/tests/common/lib.rs
      # Remove the Ok/Err match arms that follow
      sed -i '/Ok(p) => p,/d' core/tests/common/lib.rs
      sed -i '/Err(err) => panic!/,/),/d' core/tests/common/lib.rs
      sed -i '/^[[:space:]]*};$/d' core/tests/common/lib.rs || true

      # Fix all test files with multiline .context() calls on PathBuf
      # Pattern: PathBuf::from("stub-binary")\n.context(...)?  ->  PathBuf::from("stub-binary");
      for f in mcp-server/tests/common/mcp_process.rs app-server/tests/common/mcp_process.rs; do
        if [ -f "$f" ]; then
          sed -i 'N; s/std::path::PathBuf::from("stub-binary")\n[[:space:]]*\.context([^)]*)?;/std::path::PathBuf::from("stub-binary");/g; P; D' "$f"
        fi
      done

      # Also fix any .expect() on next line patterns in all test files
      find . -path "*/tests/*" -name "*.rs" -exec sed -i 'N; s/std::path::PathBuf::from("stub-binary")\n[[:space:]]*\.expect([^)]*),/std::path::PathBuf::from("stub-binary"),/g; P; D' {} \;
    '';

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
    ];

  buildInputs =
    with pkgs;
    [
      ramaBoringssl
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      openssl # for openssl-sys crate
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
