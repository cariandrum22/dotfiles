{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    shortcut = "t";
    terminal = "screen-256color";
    historyLimit = 100000;
    extraConfig = ''
      # Support 256 color
      set -g terminal-overrides 'xterm:colors=256'

      # Activate mouse
      set -g mouse on
      bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e; send-keys -M'"

      # Clipboard integration for Linux
      bind-key -T copy-mode C-w send-keys -X copy-pipe-and-cancel "xsel -i -p && xsel -o -p | xsel -i -b"
      bind-key -T copy-mode M-w send-keys -X copy-pipe-and-cancel "xsel -i -p && xsel -o -p | xsel -i -b"
      bind-key -T copy-mode MouseDragEnd1Pane send -X copy-pipe-and-cancel "xsel -i -p && xsel -o -p | xsel -i -b"
      bind-key -T root C-y run "xsel -o | tmux load-buffer - ; tmux paste-buffer"

      # Managing plugins
      set -g @plugin 'tmux-plugins/tpm'
      set -g @plugin 'tmux-plugins/tmux-sensible'
      set -g @plugin "arcticicestudio/nord-tmux"

      run '~/.tmux/plugins/tpm/tpm'
    '';
  };

  home.file.".tmux/plugins/tpm" = {
    source = pkgs.fetchFromGitHub {
      owner = "tmux-plugins";
      repo = "tpm";
      rev = "master";
      sha256 = "sha256-hW8mfwB8F9ZkTQ72WQp/1fy8KL1IIYMZBtZYIwZdMQc=";
    };
  };
}
