{
  programs.home-manager = {
    enable = true;
    path = "https://github.com/rycee/home-manager/archive/release-20.09.tar.gz";
  };

  #manual.manpages.enable = false;

  imports = [
    ./home/default.nix
    ./programs/default.nix
    ./services/picom.nix
  ];

  xresources.properties = {
    "urxvt*foreground" = "#d3d3d3";
    "urxvt*background" = "#394939";
    "urxvt*color0" = "#333333";
    "urxvt*color1" = "#cc827b";
    "urxvt*color2" = "#99e699";
    "urxvt*color3" = "#cccc66";
    "urxvt*color4" = "#9999e6";
    "urxvt*color5" = "#e666e6";
    "urxvt*color6" = "#99cce6";
    "urxvt*color7" = "#e6e6e6";
    "urxvt*color8" = "#444444";
    "urxvt*color9" = "#e68686";
    "urxvt*color10" = "#66e666";
    "urxvt*color11" = "#e6e666";
    "urxvt*color12" = "#b6b6e6";
    "urxvt*color13" = "#e686e6";
    "urxvt*color14" = "#66e6e6";
    "urxvt*color15" = "#e6e6e6";
  };

  xsession = {
    enable = true;
    windowManager = {
      xmonad = {
        enable = true;
        extraPackages = haskellPackages: [
          haskellPackages.dbus
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
        enableContribAndExtras = true;
        config = ../../xmonad/xmonad.hs;
      };
    };
    initExtra = ''
      eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
      export SSH_AUTH_SOCK

      export XMODIFIERS="@im=fcitx"
      export XMODIFIER="@im=fcitx"
      export GTK_IM_MODULE="fcitx"
      export QT_IM_MODULE="fcitx"
      fcitx

      xrdb -merge .Xresources
      feh --bg-scale ~/Pictures/Wallpaper/756745.jpg
    '';
  };
}
