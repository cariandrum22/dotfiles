{
  services.picom = {
    enable = true;
    fade = true;
    fadeSteps = [
      0.06 # fade-in-step
      0.06 # fade-out-step
    ];
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
      "100:class_g = '1Password' && focused"
      "100:class_g = '1Password' && !focused"
    ];
    fadeExclude = [
      "_NET_WM_NAME@:s = 'rofi'"
    ];
  };
}
