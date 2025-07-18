self: super:
# Only apply XMonad overlay on Linux systems
if super.stdenv.isLinux then
  {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: { })) (
        hself: hsuper: {
          xmonad = super.haskell.lib.doJailbreak (
            hsuper.xmonad.overrideAttrs (oldAttrs: rec {
              version = "v0.18.0";
              sha256 = "sha256-bM2pHSelf0hVhKbYwYatnAo92NNsh9rl6ywrWSwwjEo=";
              src = super.fetchFromGitHub {
                owner = "xmonad";
                repo = "xmonad";
                rev = version;
                inherit sha256;
              };
            })
          );
          xmonad-contrib = hsuper.xmonad-contrib.overrideAttrs (oldAttrs: rec {
            version = "ec5c751b35c1c9b07bd4361617f7c4076aeaa85f";
            sha256 = "sha256-S9IivsvYeWD/AtAgKUHH0amVfug0qOAPXjNvXbafXag=";
            src = super.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad-contrib";
              rev = version;
              inherit sha256;
            };
          });
          xmonad-extras = hsuper.xmonad-extras.overrideAttrs (oldAttrs: rec {
            version = "14e1c4ce9759b959c05b03aac1bb718abc50f762";
            sha256 = "sha256-9J6hfsAU8oH4l5mJ/W1p57yTxnLUh0dPkv6CXh3X9EA=";
            src = super.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad-extras";
              rev = version;
              inherit sha256;
            };
          });
          # Add myxmonad package directly
          myxmonad = hself.mkDerivation {
            pname = "myxmonad";
            version = "0.1.0.0";
            src = ../../xmonad;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = with hself; [
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
            license = self.lib.licenses.mit;
          };
        }
      );
    });

    # Export myxmonad at the top level for easy access
    inherit (self.haskellPackages) myxmonad;
  }
else
  { }
