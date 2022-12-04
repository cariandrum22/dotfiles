{
  services.picom = {
    enable = true;
    fade = true;
    fadeSteps = [
      0.06 # fade-in-step
      0.06 # fade-out-step
    ];
    shadow = true;
    shadowOffsets = [
      (-4) # shadow-offset-x
      (-4) # shadow-offset-y
    ];
    shadowOpacity = 0.9;
    inactiveOpacity = 0.8;
    opacityRules = [
      "100:class_g = 'Google-chrome' && focused"
      "100:class_g = 'Google-chrome' && !focused"
      "100:class_g = 'Evince' && focused"
      "100:class_g = 'Evince' && !focused"
      "100:class_g = 'Code' && focused"
      "100:class_g = 'Code' && !focused"
      "100:class_g = 'Slack' && focused"
      "100:class_g = 'Slack' && !focused"
    ];
    fadeExclude = [
      # For using rofi
      "_NET_WM_NAME@:s = 'rofi'"
    ];
    settings = {
      # Anti flickerin
      unredir-if-possible = false;

      # shadows extra
      clear-shadow   = true;
    };
  };
}
