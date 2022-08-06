let
  isDarwin = if builtins.currentSystem == "x86_64-darwin" then true else false;
  username = "claude";
  homeDirectory =
    if isDarwin then "/Users/${username}" else "/home/${username}";
  base = [ ./home/default.nix ./programs/default.nix ];
  linux = base ++ [
    ./xsession.nix
    ./home/packages/linux-desktop.nix
    ./services/picom.nix
    ./services/keybase.nix
    ./services/vscode-server.nix
  ];
  configurations = if isDarwin then base else linux;

in {
  home = {
    username = username;
    homeDirectory = homeDirectory;
    stateVersion = "22.05";
  };

  imports = configurations;

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
}
