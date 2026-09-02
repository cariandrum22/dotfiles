{ pkgs, ... }:

let
  # Build BoringSSL from rama-boring-sys source with rama's patches applied
  ramaBoringssl = pkgs.stdenv.mkDerivation rec {
    pname = "rama-boringssl";
    # rama-boring-sys version from codex's Cargo.lock (not the codex git tag)
    version = "0.5.10";

    # Download the immutable crate archive, which includes the BoringSSL source and patches.
    # The crates.io API redirect rejects non-browser fetchers with HTTP 403.
    src = pkgs.fetchurl {
      url = "https://static.crates.io/crates/rama-boring-sys/rama-boring-sys-${version}.crate";
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

  # Codex tracks recent Rust dependencies closely. Use the unstable Rust
  # toolchain on every platform so dependency MSRV bumps do not break Darwin
  # while Linux is still using a newer compiler.
  rustPlatform = pkgs.makeRustPlatform {
    inherit (pkgs.unstable) cargo rustc;
  };

  # Replace the upstream runfiles git dependency with a tiny local stub.
  # rules_rust ships examples that use -Z bindeps, which breaks nixpkgs'
  # cargo vendor utility. codex-utils-cargo-bin is only used by test helpers,
  # and tests are disabled for this package, so the stub is sufficient.
  cargoHashes = {
    x86_64-linux = "sha256-IBpKF38uVVLWeQCe5J7GXggK0WBrZhhQ4w/z+nLD3V4=";
    aarch64-linux = "sha256-IBpKF38uVVLWeQCe5J7GXggK0WBrZhhQ4w/z+nLD3V4=";
    x86_64-darwin = "sha256-IBpKF38uVVLWeQCe5J7GXggK0WBrZhhQ4w/z+nLD3V4=";
    aarch64-darwin = "sha256-IBpKF38uVVLWeQCe5J7GXggK0WBrZhhQ4w/z+nLD3V4=";
  };

  rustyV8Version = "150.4.0";

  # `codex-code-mode-runtime` turns on the v8 crate's `v8_enable_sandbox`
  # feature (which implies pointer compression). denoland/rusty_v8 publishes no
  # prebuilts for that flavor, so upstream Codex builds them itself and attaches
  # them to its own `rusty-v8-v*` release. Mirror what upstream's
  # `.github/actions/setup-rusty-v8` does and consume the same artifacts.
  rustyV8Profile = "ptrcomp_sandbox_release";

  rustyV8BaseUrl = "https://github.com/openai/codex/releases/download/rusty-v8-v${rustyV8Version}";

  rustyV8Targets = {
    x86_64-linux = "x86_64-unknown-linux-gnu";
    aarch64-linux = "aarch64-unknown-linux-gnu";
    x86_64-darwin = "x86_64-apple-darwin";
    aarch64-darwin = "aarch64-apple-darwin";
  };

  rustyV8Target = rustyV8Targets.${pkgs.stdenv.system};

  rustyV8ArchiveHashes = {
    x86_64-linux = "sha256-o1x10fJuapg4haRbM0kKTr5U8FBQVosyuJz7QhswtYM=";
    aarch64-linux = "sha256-0VF+7UBUaFNwKbAF1f6ZfsdNXI01H5FrOm3yC30oEbo=";
    x86_64-darwin = "sha256-4Nm7ZOizoDTCkwyDly8/NXYCERSDQvoEB7OCUO8zCFY=";
    aarch64-darwin = "sha256-AK27SHmISMd1UEQcaGc6XoUpuOG3PqvN7iMss5tA9KE=";
  };

  rustyV8SrcBindingHashes = {
    x86_64-linux = "sha256-dyeCauR5vbZF6Acjn7EtH44uI956bPFvXuWSaQ0dhQY=";
    aarch64-linux = "sha256-dyeCauR5vbZF6Acjn7EtH44uI956bPFvXuWSaQ0dhQY=";
    x86_64-darwin = "sha256-ylrfDPicmnCtRgrnNkiy/om3SqETs8t/dXtqArdYOU8=";
    aarch64-darwin = "sha256-ylrfDPicmnCtRgrnNkiy/om3SqETs8t/dXtqArdYOU8=";
  };

  rustyV8Archive = pkgs.fetchurl {
    url = "${rustyV8BaseUrl}/librusty_v8_${rustyV8Profile}_${rustyV8Target}.a.gz";
    hash = rustyV8ArchiveHashes.${pkgs.stdenv.system};
  };

  # The v8 crate `include!`s the generated bindings instead of running bindgen,
  # and only vendors the flavors it publishes itself. Supply the matching
  # sandbox bindings so the build never reaches for the network.
  rustyV8SrcBinding = pkgs.fetchurl {
    url = "${rustyV8BaseUrl}/src_binding_${rustyV8Profile}_${rustyV8Target}.rs";
    hash = rustyV8SrcBindingHashes.${pkgs.stdenv.system};
  };

  livekitWebRtcTag = "webrtc-24f6822-2";

  livekitWebRtcTriples = {
    x86_64-linux = "linux-x64-release";
    aarch64-linux = "linux-arm64-release";
    x86_64-darwin = "mac-x64-release";
    aarch64-darwin = "mac-arm64-release";
  };

  livekitWebRtcZipHashes = {
    x86_64-linux = "sha256-89SaZMN+qJmvUt3GhfUx8Kvi+3VSiqTa4lKtqqA77Mw=";
    aarch64-linux = "sha256-QBPVPoY+RwQt1Ztnsb2EltoER6yEw9cMFwSZQG8Tqgs=";
    x86_64-darwin = "sha256-COQh7Wa0KEmM1qUTMMldmP7WncRKPBNJ7RaiRowUyV8=";
    aarch64-darwin = "sha256-eb5cwV5uBjPEOA4z4XLX6/Gm3Og+ngmXYdYQPw1+tsE=";
  };

  livekitWebRtcZip = pkgs.fetchurl {
    url = "https://github.com/livekit/rust-sdks/releases/download/${livekitWebRtcTag}/webrtc-${
      livekitWebRtcTriples.${pkgs.stdenv.system}
    }.zip";
    hash = livekitWebRtcZipHashes.${pkgs.stdenv.system};
  };

  livekitWebRtcArchive =
    pkgs.runCommand "livekit-webrtc-${livekitWebRtcTriples.${pkgs.stdenv.system}}"
      {
        nativeBuildInputs = [
          pkgs.unzip
        ];
      }
      ''
        extract_dir="$TMPDIR/livekit-webrtc"
        extracted_root="$extract_dir/${livekitWebRtcTriples.${pkgs.stdenv.system}}"

        mkdir -p "$extract_dir"
        unzip -qq ${livekitWebRtcZip} -d "$extract_dir"

        if [ ! -d "$extracted_root" ]; then
          echo "Expected extracted LiveKit WebRTC directory at $extracted_root" >&2
          find "$extract_dir" -maxdepth 2 -mindepth 1 -print >&2 || true
          exit 1
        fi

        mv "$extracted_root" "$out"
      '';
in
rustPlatform.buildRustPackage (
  rec {
    pname = "codex-cli";
    version = "rust-v0.152.1";

    src = pkgs.fetchFromGitHub {
      owner = "openai";
      repo = "codex";
      rev = "${version}";
      hash = "sha256-XPfKxUQACUgJ+LNTxjbBm4pNHB3NhuNci2ECovAv38w=";
    };

    sourceRoot = "source/codex-rs";

    cargoHash = cargoHashes.${pkgs.stdenv.system};

    # Build the distributed CLI plus the Code Mode host helper. Upstream's
    # workspace also contains samples and test utilities that are not part of
    # this package. Since rust-v0.147.0 the CLI spawns `codex-code-mode-host`,
    # which it resolves as a sibling of its own executable, so that binary has
    # to land in the same `bin` directory.
    cargoBuildFlags = [
      "-p"
      "codex-cli"
      "-p"
      "codex-code-mode-host"
    ];

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
    + pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
            # Apple's newer linker can overflow ARM64 branch ranges when linking the
            # WebRTC-heavy Codex binary. Force the classic linker on Darwin targets.
            cat >> .cargo/config.toml <<'EOF'

      [target.x86_64-apple-darwin]
      rustflags = ["-C", "link-arg=-ld_classic"]

      [target.aarch64-apple-darwin]
      rustflags = ["-C", "link-arg=-ld_classic"]
      EOF
    ''
    + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
      # LLVM currently crashes in release optimization passes for this
      # workspace. Lower Linux release optimization until the toolchain settles.
      perl -0pi -e 's/\\[profile\\.release\\]\\n/[profile.release]\\nopt-level = 2\\n/' Cargo.toml
    '';

    # Enable unstable features (file_lock)
    RUSTC_BOOTSTRAP = "1";

    # Disable LTO to work around LLVM ICEs during LTO codegen.
    # The crash occurs in LLVMContextDispose when building with lto=fat.
    # This affects both binary size and runtime performance (5-20% slower).
    # TODO: Re-enable LTO ("fat" or "thin") once nixpkgs updates LLVM/rustc.
    # To test: remove this line and run `nix build .#codex-cli`
    CARGO_PROFILE_RELEASE_LTO = "off";

    # Upstream pins codegen-units=1 for smaller binaries. Keep that on Darwin,
    # where we need to minimize link distance for the large WebRTC static
    # archive. Linux still needs multiple codegen units to avoid an LLVM 21 /
    # rustc SIGSEGV while compiling `image`.
    CARGO_PROFILE_RELEASE_CODEGEN_UNITS = if pkgs.stdenv.isLinux then "16" else "1";

    # Show backtrace for build failures
    RUST_BACKTRACE = "1";

    # Keep rusty_v8 fully offline inside the Nix sandbox.
    RUSTY_V8_ARCHIVE = "${rustyV8Archive}";
    RUSTY_V8_SRC_BINDING_PATH = "${rustyV8SrcBinding}";

    # Keep LiveKit WebRTC fully offline inside the Nix sandbox.
    LK_CUSTOM_WEBRTC = "${livekitWebRtcArchive}";

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

    # Ensure libc++ is linked on Darwin. The classic linker is selected via the
    # target-specific rustflags injected into `.cargo/config.toml` above.
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
  // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
    # Favor smaller release binaries on Darwin so the final WebRTC-heavy link
    # stays within ARM64 branch range limits.
    CARGO_PROFILE_RELEASE_OPT_LEVEL = "s";
  }
  // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
    # Point openssl-sys to system OpenSSL on Linux (avoid BoringSSL detection)
    OPENSSL_DIR = "${opensslPkg.out}";
    OPENSSL_LIB_DIR = "${opensslPkg.out}/lib";
    OPENSSL_INCLUDE_DIR = "${opensslPkg.dev}/include";
    OPENSSL_NO_VENDOR = "1";
  }
)
