{ pkgs, ... }:

{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        # Display
        monitor = 0;
        follow = "mouse";
        geometry = "300x60-15+46";
        indicate_hidden = "yes";
        shrink = "yes";
        transparency = 0;
        notification_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        frame_width = 0;
        frame_color = "#000000";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;
        # Text
        font = "Fira Code Nerd Font 9";
        line_height = 0;
        markup = "full";
        format = "<b>%s</b>\n%b";
        alignment = "left";
        show_age_threshold = 120;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";
        # Icons
        icon_position = "left";
        max_icon_size = 32;
        icon_path = ".icons/candy-icons/";
        # Misc/Advanced
        dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
        browser = "${pkgs.google-chrome}/bin/google-chrome-stable";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        verbosity = "mesg";
        corner_radius = 8;
        # Legacu
        force_xinerama = false;
        # mouse
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action";
        mouse_right_click = "close_all";
      };
      urgency_low = {
        background = "#D8DEE9";
        foreground = "#4C566A";
        timeout = 20;
      };
      urgency_normal = {
        background = "#434C5E";
        foreground = "#D8DEE9";
        timeout = 20;
      };
      urgency_critical = {
        background = "#BF616A";
        foreground = "#D8DEE9"; #FDF1DB #f5bfd2 #ff8585   #ffdfd3  #fec8d8      #FDF1DB #8fbcbb
        timeout = 0;
      };
    };
  };
}
