self: super:
# Only apply XMonad overlay on Linux systems
if super.stdenv.isLinux then
  {
    haskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: { })) (
        hself: hsuper: {
          xmonad = super.haskell.lib.doJailbreak (
            hsuper.xmonad.overrideAttrs (_oldAttrs: rec {
              version = "v0.18.1";
              sha256 = "sha256-Le6CanUisUmcIK2wN8AyoLECMSWOq5DBsRZuqwVWd68=";
              src = super.fetchFromGitHub {
                owner = "xmonad";
                repo = "xmonad";
                rev = version;
                inherit sha256;
              };
            })
          );
          xmonad-contrib = hsuper.xmonad-contrib.overrideAttrs (_oldAttrs: rec {
            version = "v0.18.2";
            sha256 = "sha256-xRhtmKHdUk66tg+13qJdPFMgIByUTIq8bSS5eIkOToo=";
            src = super.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad-contrib";
              rev = version;
              inherit sha256;
            };
          });
          xmonad-extras = hsuper.xmonad-extras.overrideAttrs (_oldAttrs: rec {
            version = "v0.17.3";
            sha256 = "sha256-SuxoAiXf0hyRqeexoFoga/UfkLInzFe3rf0BUoDC8XE=";
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
