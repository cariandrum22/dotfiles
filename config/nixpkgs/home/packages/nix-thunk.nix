let pkgs = import <nixpkgs> { config.allowBroken = true; };

in {
  home.packages = with pkgs;
    [
      # TODO: Get nix-thunk from haskellPackages once updated.
      (haskellPackages.extend (self: super: {
        nix-thunk = haskell.lib.doJailbreak (self.callHackageDirect {
          pkg = "nix-thunk";
          ver = "0.2.0.2";
          sha256 = "0af0days52223ahda1v8l5nassdn924yhinh8fr3madpzmiymmvp";
        } { });
        cli-extras = haskell.lib.doJailbreak (self.callHackageDirect {
          pkg = "cli-extras";
          ver = "0.1.0.1";
          sha256 = "1hnmk8jm9zrhcv8vz9raj0p26svc0rd47zc7nz58sja6jsakcaq2";
        } { });
        cli-git = haskell.lib.doJailbreak (self.callHackageDirect {
          pkg = "cli-git";
          ver = "0.1.0.1";
          sha256 = "0kqnzkz73iid5i1lcbgvdkf2pip3sapc7px42c1525cga13bhgkn";
        } { });
        cli-nix = self.callHackageDirect {
          pkg = "cli-nix";
          ver = "0.1.0.1";
          sha256 = "1kcr33w8kkyyynml0i20zdia792jprbg4av7f5dajvy4a2l2vmgj";
        } { };
        unliftio-core = haskell.lib.doJailbreak ( self.callHackageDirect {
          pkg = "unliftio-core";
          ver = "0.2.0.1";
          sha256 = "06cbv2yx5a6qj4p1w91q299r0yxv96ms72xmjvkpm9ic06ikvzzq";
        } { });
      })).nix-thunk
    ];
}
