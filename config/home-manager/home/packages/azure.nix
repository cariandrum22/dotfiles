{ pkgs, lib, ... }:

{
  home = {
    packages = lib.flatten (
      with pkgs;
      [
        # Microsoft Azure
        azure-cli
        azure-storage-azcopy
        (with azure-cli-extensions; [
          aks-preview
        ])
      ]
    );
  };
}
