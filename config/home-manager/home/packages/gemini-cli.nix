{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_22,
}:

buildNpmPackage rec {
  pname = "gemini-cli";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "google-gemini";
    repo = "gemini-cli";
    rev = "52afcb3a1233237b07aa86b1678f4c4eded70800";
    hash = "sha256-KNnfo5hntQjvc377A39+QBemeJjMVDRnNuGY/93n3zc=";
  };

  npmDepsHash = "sha256-/IAEcbER5cr6/9BFZYuV2j1jgA75eeFxaLXdh1T3bMA";

  nodejs = nodejs_22;

  npmBuildScript = "bundle";

  postPatch = ''
    substituteInPlace scripts/generate-git-commit-info.js \
      --replace-fail "import { execSync } from 'child_process';" \
      "const execSync = (cmd) => cmd.includes(\"rev-parse\") ? \"${builtins.substring 0 7 src.rev}\" : \"\";"
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/node_modules/@google/gemini-cli
    cp -r bundle package.json LICENSE README.md $out/lib/node_modules/@google/gemini-cli/

    mkdir -p $out/bin
    ln -s $out/lib/node_modules/@google/gemini-cli/bundle/gemini.js $out/bin/gemini

    runHook postInstall
  '';

  meta = {
    description = "An open-source AI agent that brings the power of Gemini directly into your terminal.";
    homepage = "https://github.com/google-gemini/gemini-cli";
    license = lib.licenses.asl20;
    mainProgram = "gemini";
  };
}
