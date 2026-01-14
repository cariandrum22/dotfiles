# Workarounds for nixpkgs 25.11 build failures on aarch64-darwin
#
# These overrides use unstable packages to avoid test failures that only
# occur on macOS aarch64. Remove this overlay when the issues are fixed
# upstream in nixpkgs.
#
# Known issues (as of 2026-01):
# - LLVM 20.1.8: 1 test failure on macOS (blocks clang-tools, dotnet, postgresql)
# - nix 2.28.5: nix-shell test failure (blocks cachix)
# - setproctitle 1.3.7: fork test segfault (blocks azure-cli, glances)
# - pre-commit 4.3.0: requires dotnet for tests (triggers LLVM build)
#
# Note: tlaps is excluded on macOS via lib.optionals in default.nix
# (vampire-5.0.0 build fails with clang on macOS)
_final: prev:
if prev.stdenv.isDarwin && prev.stdenv.hostPlatform.isAarch64 then
  {
    inherit (prev.unstable)
      # Packages with LLVM 20.1.8 test failure dependency
      clang-tools
      dotnet-sdk
      postgresql
      # Packages with nix 2.28.5 test failure dependency
      cachix
      # Packages with setproctitle test failure dependency
      azure-cli
      azure-cli-extensions
      glances
      # pre-commit requires dotnet for tests, which triggers LLVM build
      pre-commit
      ;
  }
else
  { }
