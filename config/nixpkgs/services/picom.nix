{
  services.picom = {
    enable = true;
    fade = true;
    fadeSteps = [
      "0.06" # fade-in-step
      "0.06" # fade-out-step
    ];
    shadow = true;
    shadowOffsets = [
      (-4) # shadow-offset-x
      (-4) # shadow-offset-y
    ];
    shadowOpacity = "0.9";
    inactiveOpacity = "0.8";
    opacityRule = [
      "100:class_g = 'google-chrome' && focused"
      "100:class_g = 'google-chrome' && !focused"
    ];
    extraOptions = ''
      # For using rofi
      focus-exclude = "_NET_WM_NAME@:s = 'rofi'"

      # shadows extra
      clear-shadow   = true;
      shadow-radius   = 4;
    '';
  };
}
