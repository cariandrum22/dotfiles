{ pkgs, lib, ... }:

let
  # Keep the existing setproctitle workaround on Apple Silicon.
  basePkgs =
    if pkgs.stdenv.isDarwin && pkgs.stdenv.hostPlatform.isAarch64 then pkgs.unstable else pkgs;
in
# Backport nixpkgs' migration from insecure python-jose to PyJWT.
# Remove this override once both pinned channels provide Glances >= 4.5.9.
# https://github.com/NixOS/nixpkgs/commit/1ae7f1bd54f9ad49fa71ca69e1a13d511aef01ac
basePkgs.glances.overridePythonAttrs (old: {
  version = "4.5.9";
  src = basePkgs.fetchFromGitHub {
    owner = "nicolargo";
    repo = "glances";
    tag = "v4.5.9";
    hash = "sha256-CvT9GQ29xXTVxC7V2nGmfDhHLsAtF61+XMMLG8cmHww=";
  };

  dependencies = lib.filter (dep: lib.getName dep != "python-jose") old.dependencies ++ [
    basePkgs.python3Packages.pyjwt
  ];
  nativeCheckInputs = old.nativeCheckInputs ++ [ basePkgs.python3Packages.pysmart ];
})
