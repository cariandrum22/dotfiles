{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "rivendell";
  version = "unstable-2021-09-20";

  src = pkgs.fetchFromGitHub {
    owner = "crinklywrappr";
    repo = "rivendell";
    rev = "60db95773cc8cce96ce4e3eed48ae628c434c4f5";
    hash = "sha256-yN5Hq+tzxQ+yQYOCuku0W80ffmytGB8inlW2owxo91w=";
  };

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/elvish/lib/rivendell

    # Copy all .elv module files to the elvish lib directory
    cp *.elv $out/share/elvish/lib/rivendell/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Elvish shell library with functional programming utilities, lazy iterators, and visualizations";
    homepage = "https://github.com/crinklywrappr/rivendell";
    license = licenses.bsd2;
  };
}
