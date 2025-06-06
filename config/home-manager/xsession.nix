{
  xsession = {
    enable = true;
    windowManager = {
      xmonad = {
        enable = true;
        extraPackages =
          haskellPackages: with haskellPackages; [
            dbus
            xmonad-contrib
            xmonad-extras
          ];
        enableContribAndExtras = true;
        config = ../../xmonad/xmonad.hs;
      };
    };
    initExtra = ''
      eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
      export SSH_AUTH_SOCK

      export XMODIFIERS="@im=fcitx"
      export GTK_IM_MODULE="fcitx"
      export QT_IM_MODULE="fcitx"
      export GLFW_IM_MODULE="ibus"
      fcitx5 &

      xrdb -merge .Xresources
      feh --bg-scale ~/Pictures/Wallpaper/756745.jpg
    '';
  };
}
