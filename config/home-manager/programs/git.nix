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
  Claude = [
    ".claude/"
  ];
  Agents = [
    ".agents"
  ];
  Markdown = [
    # Markdown files
    "*.md"
    "*.markdown"
    "*.mkd"
    "*.mdown"
    "*.mdwn"
    "*.mdtxt"
    "*.mdtext"
    "!README.md"
  ];
in
{
  programs.git = {
    enable = true;
    userName = "Takafumi Asano";
    userEmail = "cariandrum22@gmail.com";
    signing = {
      key = "48F302B631525FA4D5A40106AC7AA4174AC64BA5";
      signByDefault = true;
    };
    lfs.enable = true;
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
      };
      credential = {
        helper = "netrc -f ~/.netrc.gpg -v";
      };
      color = {
        ui = true;
      };
      init = {
        defaultBranch = "main";
      };
      ghq = {
        root = [
          "~/Go/src"
          "~/Codex"
        ];
      };
      pull = {
        rebase = true;
      };
    };
    ignores = builtins.concatLists [
      Linux
      macOS
      Windows
      Claude
      Agents
      Markdown
    ];
  };
}
