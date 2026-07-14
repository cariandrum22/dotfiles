{
  fetchurl,
  lib,
  makeBinaryWrapper,
  nodejs,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "prettier";
  version = "3.8.3";

  # The published package is already built and avoids nixpkgs' pnpm build chain.
  src = fetchurl {
    url = "https://registry.npmjs.org/prettier/-/prettier-${finalAttrs.version}.tgz";
    hash = "sha256-yKhQ5xtzZvG6wYheBbL5KdRxaACfO2kUQTkCyouYD94=";
  };

  nativeBuildInputs = [ makeBinaryWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin" "$out/lib/node_modules/prettier"
    cp -R ./. "$out/lib/node_modules/prettier/"
    makeWrapper ${lib.getExe nodejs} "$out/bin/prettier" \
      --add-flags "$out/lib/node_modules/prettier/bin/prettier.cjs"

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    test "$("$out/bin/prettier" --version)" = "${finalAttrs.version}"
    runHook postInstallCheck
  '';

  meta = {
    description = "Opinionated code formatter";
    homepage = "https://prettier.io";
    license = lib.licenses.mit;
    mainProgram = "prettier";
  };
})
