{
  description = "Custom XMonad configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-linux"
      ]
      (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs) haskellPackages;

          myxmonad = haskellPackages.mkDerivation {
            pname = "myxmonad";
            version = "0.1.0.0";
            src = ./.;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = with haskellPackages; [
              base
              Cabal
              containers
              unordered-containers
              utf8-string
              X11
              xmonad
              xmonad-contrib
              xmonad-extras
            ];
            license = pkgs.lib.licenses.mit;
          };
        in
        {
          packages = {
            default = myxmonad;
            inherit myxmonad;
          };

          apps.default = flake-utils.lib.mkApp { drv = myxmonad; };

          devShells.default = haskellPackages.shellFor {
            packages = ps: [ myxmonad ];
            buildInputs = with haskellPackages; [
              cabal-install
              ghc
              haskell-language-server
              ormolu
              hlint
            ];
          };
        }
      )
    // {
      # System-agnostic overlay
      overlays.default = final: prev: {
        myxmonad = self.packages.${prev.system}.default;
      };
    };
}
