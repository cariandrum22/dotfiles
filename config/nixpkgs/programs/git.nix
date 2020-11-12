let
  # Copy from https://github.com/github/gitignore
  Linux = [
    "*~"
    # temporary files which can be created if a process still has a handle open of a deleted file
    ".fuse_hidden*"
    # KDE directory preferences
    ".directory"
    # Linux trash folder which might appear on any partition or disk
    ".Trash-*"
    # .nfs files are created when an open file is removed but is still being accessed
    ".nfs*"
  ];
  macOS = [
    # General
    ".DS_Store"
    ".AppleDouble"
    ".LSOverride"

    # Icon must end with two \r
    "Icon"

    # Thumbnails
    "._*"

    # Files that might appear in the root of a volume
    ".DocumentRevisions-V100"
    ".fseventsd"
    ".Spotlight-V100"
    ".TemporaryItems"
    ".Trashes"
    ".VolumeIcon.icns"
    ".com.apple.timemachine.donotpresent"
    # Directories potentially created on remote AFP share
    ".AppleDB"
    ".AppleDesktop"
    "Network Trash Folder"
    "Temporary Items"
    ".apdisk"
  ];
  Windows = [
    # Windows thumbnail cache files
    "Thumbs.db"
    "Thumbs.db:encryptable"
    "ehthumbs.db"
    "ehthumbs_vista.db"

    # Dump file
    "*.stackdump"

    # Folder config file
    "[Dd]esktop.ini"

    # Recycle Bin used on file shares
    "$RECYCLE.BIN/"

    # Windows Installer files
    "*.cab"
    "*.msi"
    "*.msix"
    "*.msm"
    "*.msp"

    # Windows shortcuts
    "*.lnk"
  ];
  Emacs = [
    # -*- mode: gitignore; -*-
    "*~"
    "\#*\#"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "*.elc"
    "auto-save-list"
    "tramp"
    ".\#*"

    # Org-mode
    ".org-id-locations"
    "*_archive"

    # flymake-mode
    "*_flymake.*"

    # eshell files
    "/eshell/history"
    "/eshell/lastdir"

    # elpa packages
    "/elpa/"

    # reftex files
    "*.rel"

    # AUCTeX auto folder
    "/auto/"

    # cask packages
    ".cask/"
    "dist/"

    # Flycheck
    "flycheck_*.el"

    # server auth directory
    "/server/"

    # projectiles files
    ".projectile"

    # directory configuration
    ".dir-locals.el"

    # network security
    "/network-security.data"
  ];


in {
  programs.git = {
    enable = true;
    userName = "Talafumi Asano";
    userEmail = "cariandrum22@gmail.com";
    signing = {
      key = "48F302B631525FA4D5A40106AC7AA4174AC64BA5";
      signByDefault = true;
    };
    aliases = {
      co = "checkout";
      br = "branch";
      cl = "clone";
      pl = "pull";
      fc = "fetch";
      st = "stash";
    };
    extraConfig = {
      core = {
        editor = "emacs";
        excludesfile = "~/.gitignore";
      };
      credential = {
        helper = "netrc -f ~/.netrc.gpg -v";
      };
      color = {
        ui = true;
      };

      ghq = {
        root = "~/Codex";
        # TODO: Add ghq support to the home-manager git module
        # NOTE: ghq can specify multiple search paths, but the keys are
        #       duplicated.
        #       This is not handled by the nix set type, so you need to
        #       consider taking it as a list and reflecting it in gitconfig
        #       or something like that.
        #root = "~/go/src";
      };
    };
    ignores = builtins.concatLists [Linux macOS Windows Emacs];
  };
}
