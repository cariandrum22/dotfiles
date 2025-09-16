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
      # Start gnome-keyring without SSH component (SSH is handled by 1Password)
      eval $(gnome-keyring-daemon --start --components=pkcs11,secrets)

      # Use 1Password SSH agent
      export SSH_AUTH_SOCK="$HOME/.1password/agent.sock"

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
