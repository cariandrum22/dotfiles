{ config, pkgs, ... }:

let colorScheme = import ../lib/theme/nord.nix;
in {
  programs.rofi = {
    enable = true;
    plugins = with pkgs; [ rofi-calc rofi-emoji ];
    font = "Fira Code Nerd Font 12";
    terminal = "kitty";
    theme =
      let inherit (config.lib.formats.rasi) mkLiteral;
      in with colorScheme; {
        "*" = {
          margin = 0;
          padding = 0;
          spacing = 0;
          border = 0;

          text-color = mkLiteral nord4;
          background-color = mkLiteral "transparent";

          background = mkLiteral nord0;
          foreground = mkLiteral nord4;

          normal-background = mkLiteral "@background";
          normal-foreground = mkLiteral "@foreground";
          alternate-normal-background = mkLiteral "@normal-background";
          alternate-normal-foreground = mkLiteral "@normal-foreground";
          selected-normal-background = mkLiteral nord10;
          selected-normal-foreground = mkLiteral nord4;

          active-background = mkLiteral nord3;
          active-foreground = mkLiteral nord10;
          alternate-active-background = mkLiteral "@active-background";
          alternate-active-foreground = mkLiteral "@active-foreground";
          selected-active-background = mkLiteral nord10;
          selected-active-foreground = mkLiteral nord4;

          urgent-background = mkLiteral nord3;
          urgent-foreground = mkLiteral nord11;
          alternate-urgent-background = mkLiteral "@urgent-background";
          alternate-urgent-foreground = mkLiteral "@urgent-foreground";
          selected-urgent-background = mkLiteral nord11;
          selected-urgent-foreground = mkLiteral nord3;
        };
        "#window" = {
          width = 700;
        };
        "#inputbar" = {
          children = mkLiteral ''
            [ prompt
            , textbox-prompt-colon
            , entry
            , num-filtered-rows
            , textbox-sep
            , num-rows
            , case-indicator
            ]
          '';
          background-color = mkLiteral nord2;
          padding = mkLiteral "6 4";
        };
        "#prompt" = { text-color = mkLiteral nord8; };
        "#textbox-prompt-colon" = {
          expand = false;
          margin = mkLiteral "0 12";
          str = mkLiteral "\":\"";
        };
        "#num-filtered-rows" = {
          text-color = mkLiteral nord14;
        };
        "#textbox-sep" = {
          expand = false;
          margin = mkLiteral "0 4";
          str = mkLiteral "\"/\"";
        };
        "#case-indicator" = {
          text-color = mkLiteral nord14;
        };
        "#entry" = {
          padding = mkLiteral "0 12 0 0";
          text-color = mkLiteral nord5;
        };
        "#message" = { padding = mkLiteral "2 0 0"; };
        "#textbox" = { text-color = mkLiteral "@foreground"; };
        "#listview" = {
          background-color = mkLiteral "@background";
        };
        "#element" = {
          padding = mkLiteral "0 2";
          spacing = 2;
        };
        "#element normal.normal" = {
          text-color = mkLiteral "@normal-foreground";
          background-color = mkLiteral "@normal-background";
        };
        "#element normal.urgent" = {
          text-color = mkLiteral "@urgent-foreground";
          background-color = mkLiteral "@urgent-background";
        };
        "#element normal.active" = {
          text-color = mkLiteral "@active-foreground";
          background-color = mkLiteral "@active-background";
        };
        "#element selected.normal" = {
          text-color = mkLiteral "@selected-normal-foreground";
          background-color = mkLiteral "@selected-normal-background";
        };
        "#element selected.urgent" = {
          text-color = mkLiteral "@selected-urgent-foreground";
          background-color = mkLiteral "@selected-urgent-background";
        };
        "#element selected.active" = {
          text-color = mkLiteral "@selected-active-foreground";
          background-color = mkLiteral "@selected-active-background";
        };
        "#element alternate.normal" = {
          text-color = mkLiteral "@alternate-normal-foreground";
          background-color = mkLiteral "@alternate-normal-background";
        };
        "#element alternate.urgent" = {
          text-color = mkLiteral "@alternate-urgent-foreground";
          background-color = mkLiteral "@alternate-urgent-background";
        };
        "#element alternate.active" = {
          text-color = mkLiteral "@alternate-active-foreground";
          background-color = mkLiteral "@alternate-active-background";
        };
        "#button selected" = {
          background-color = mkLiteral "@selected-normal-background";
          text-color = mkLiteral "@selected-normal-foreground";
        };
      };
    extraConfig = {
      no-lazy-grab = true;
      show-icons = true;
    };
  };
}
