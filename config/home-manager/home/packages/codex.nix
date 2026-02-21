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

  # Remove utils/cargo-bin from workspace to avoid rules_rust git dependency.
  # The utils/cargo-bin crate depends on 'runfiles' which comes from
  # dzbarsky/rules_rust. That repo contains examples using -Z bindeps,
  # which causes nixpkgs 25.11's cargo vendor utility to fail when parsing.
  cargoHashes = {
    x86_64-linux = "sha256-beuNqeetNviC83LFSV3lWi3nuw/oxW0O8QXHZCJK34o=";
    aarch64-darwin = "sha256-kBg8LAI01QcWnX9oSEhYRsMP3sFmEiSa5B0tey8CnbM=";
  };
in
pkgs.rustPlatform.buildRustPackage (
  rec {
    pname = "codex-cli";
    version = "rust-v0.104.0";

    src = pkgs.fetchFromGitHub {
      owner = "openai";
      repo = "codex";
      rev = "${version}";
      hash = "sha256-spWb/msjl9am7E4UkZfEoH0diFbvAfydJKJQM1N1aoI=";
    };

    sourceRoot = "source/codex-rs";

    cargoHash = cargoHashes.${pkgs.stdenv.system};

    cargoPatches = [
      ./remove-cargo-bin.patch
    ];

    cargoBinFixes = ''
      # Drop codex-utils-cargo-bin/runfiles from manifests and lockfile
      perl -i -ne 'print unless /"utils\/cargo-bin"/ || /^\s*runfiles\s*=\s*{ git = "https:\/\/github.com\/dzbarsky\/rules_rust"/' Cargo.toml
      find . -name "Cargo.toml" -exec perl -i -ne 'print unless /codex-utils-cargo-bin/' {} \;
      if [ -f Cargo.lock ]; then
        perl -0pi -e '
          s/\n\[\[package\]\]\nname = "codex-utils-cargo-bin".*?(?=\n\[\[package\]\]|\n\z)//s;
          s/\n\[\[package\]\]\nname = "runfiles".*?(?=\n\[\[package\]\]|\n\z)//s;
          s/^\s*"codex-utils-cargo-bin",\n//mg;
        ' Cargo.lock
      fi
    '';

    # Remove codex-utils-cargo-bin references from manifests/lockfile and
    # then stub any remaining callsites in code. Tests are disabled
    # (doCheck = false) so stub implementations work fine.
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
          # Replace repo_root()? calls with a direct stub PathBuf
          s/codex_utils_cargo_bin::repo_root\(\)\?/std::path::PathBuf::from(".")/g;
          # Replace repo_root() calls with explicit Ok type to preserve .ok() chaining
          s/codex_utils_cargo_bin::repo_root\(\)/Ok::<std::path::PathBuf, std::io::Error>(std::path::PathBuf::from("."))/g;
          # Replace find_resource! macro calls - use direct PathBuf
          s/codex_utils_cargo_bin::find_resource!\([^)]+\)\?/std::path::PathBuf::from("stub-resource")/g;
          s/codex_utils_cargo_bin::find_resource!\([^)]+\)/std::path::PathBuf::from("stub-resource")/g;
          s/find_resource!\([^)]+\)\?/std::path::PathBuf::from("stub-resource")/g;
          s/find_resource!\([^)]+\)/std::path::PathBuf::from("stub-resource")/g;
        '';
      in
      ''
        ${cargoBinFixes}

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
