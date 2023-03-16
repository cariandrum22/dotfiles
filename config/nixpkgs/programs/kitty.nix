{
  programs.kitty = {
    enable = true;
    settings = {
      font_family = "Fira Code Retina Nerd Font Complete Mono";
      font_size = "12.0";
      /*
        Nord Colorscheme for Kitty
        Based on:
        - https://gist.github.com/marcusramberg/64010234c95a93d953e8c79fdaf94192
        - https://github.com/arcticicestudio/nord-hyper
      */
      foreground = "#D8DEE9";
      background = "#2E3440";
      selection_foreground = "#000000";
      selection_background = "#FFFACD";
      url_color = "#0087BD";
      cursor = "#81A1C1";
      # black
      color0 = "#3B4252";
      color8 = "#4C566A";
      # red
      color1 = "#BF616A";
      color9 = "#BF616A";
      # green
      color2 = "#A3BE8C";
      color10 = "#A3BE8C";
      # yellow
      color3 = "#EBCB8B";
      color11 = "#EBCB8B";
      # blue
      color4 = "#81A1C1";
      color12 = "#81A1C1";
      # magenta
      color5 = "#B48EAD";
      color13 = "#B48EAD";
      # cyan
      color6 = "#88C0D0";
      color14 = "#8FBCBB";
      # white
      color7 = "#E5E9F0";
      color15 = "#ECEFF4";

      background_opacity = "0.9";

      remember_window_size = "no";
      initial_window_width = 640;
      initial_window_height = 400;

      kitty_mod = "ctrl+shift";
      scrollback_lines = 100000;
      confirm_os_window_close = 0;

      macos_option_as_alt = true;
      macos_quit_when_last_window_closed = true;
    };
    keybindings = {
      "f1" = "launch --allow-remote-control kitty +kitten broadcast --match-tab state:focused";
    };
  };
}
