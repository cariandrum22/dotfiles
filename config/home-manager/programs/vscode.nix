{ lib, pkgs, ... }:

let
  unstable = import <unstable> { config.allowUnfree = true; };
in
{
  programs.vscode = {
    enable = true;
    package = unstable.vscode;
    extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "cpptools";
        publisher = "ms-vscode";
        version = "1.16.0";
        sha256 = "sha256-bHrbdsGb9VW/R2rKGtTg6mov/BHJhRkHljAzIsvSpP4=";
      }
      {
        name = "cpptools-themes";
        publisher = "ms-vscode";
        version = "2.0.0";
        sha256 = "sha256-YWA5UsA+cgvI66uB9d9smwghmsqf3vZPFNpSCK+DJxc=";
      }
      {
        name = "cmake";
        publisher = "twxs";
        version = "0.0.17";
        sha256 = "sha256-CFiva1AO/oHpszbpd7lLtDzbv1Yi55yQOQPP/kCTH4Y=";
      }
      {
        name = "cmake-tools";
        publisher = "ms-vscode";
        version = "1.15.12";
        sha256 = "sha256-1gVx7QAQPN/lcQCfmR1e5Yf+UFtaHQ+eWhxyr4FIvIQ=";
      }
    ];
  };
}
