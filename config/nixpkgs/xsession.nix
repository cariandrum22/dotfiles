{
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
