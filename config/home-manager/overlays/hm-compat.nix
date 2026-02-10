# Fixes for nixpkgs package path changes and compatibility issues
#
# These overlays provide compatibility aliases for packages that have
# been moved or renamed in nixpkgs but are still referenced by their
# old paths in some modules (e.g., Home Manager).
_final: prev: {
  # Home Manager's xresources module uses pkgs.xrdb but the package
  # is actually at pkgs.xorg.xrdb in nixpkgs
  inherit (prev.xorg) xrdb;
}
