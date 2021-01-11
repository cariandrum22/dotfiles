self: super:
let
  nixopsLibvirtdSrc = self.fetchFromGitHub {
    owner = "nix-community";
    repo = "nixops-libvirtd";
    sha256 = "0g2ag4mhgrxws3h4q8cvfh4ks1chgpjm018ayqd48lagyvi32l8m";
    rev = "1c29f6c716dad9ad58aa863ebc9575422459bf95";
  };
  nixopsLibvirtdPlugin = self.callPackage "${nixopsLibvirtdSrc}/release.nix" {};
  nixopsSrc = self.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixops";
    sha256 = "0irf9wha2rxla6z7mywj5z29bvjbpwlxqj2s29ygsbhp6hnlbzzz";
    rev = "4cfb70513bad149183adc3ac741c176d83b0e9d5";
  };
  nixopsPlugins = _: [ nixopsLibvirtdPlugin ];

in
{
  nixops = if builtins.currentSystem == "x86_64-linux"
             then (self.callPackage "${nixopsSrc}/release.nix" { p = nixopsPlugins; }).build.x86_64-linux
             else super.nixops;
}
