{
  lib,
  buildNpmPackage,
  fetchurl,
  nodejs_22,
}:

buildNpmPackage rec {
  pname = "even-terminal";
  version = "0.8.1";

  src = fetchurl {
    url = "https://registry.npmjs.org/@evenrealities/even-terminal/-/even-terminal-${version}.tgz";
    hash = "sha256-VPFMXoH8f4APRHn4Y2hy5+L14m7TJ74cTW88e6oCD3U=";
  };

  nodejs = nodejs_22;
  npmDepsHash = "sha256-BZ0Vc4SShJSIFNKWCu1KT2AYOfFnwrBlFLYv+N7ZAk0=";

  patches = [ ./bind-interface.patch ];

  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  meta = {
    description = "AI coding terminal for Even Realities smart glasses";
    homepage = "https://www.npmjs.com/package/@evenrealities/even-terminal";
    license = lib.licenses.unfree;
    mainProgram = "even-terminal";
    platforms = lib.platforms.unix;
  };
}
