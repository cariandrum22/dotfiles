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

      # Hybrid clipboard integration: OSC 52 + xsel for maximum compatibility
      # OSC 52 works over SSH and in terminals that support it
      # xsel provides integration with system clipboard (Chrome, Firefox, etc.)
      set -g set-clipboard on

      # Allow OSC 52 to set the clipboard
      set -g allow-passthrough on

      # Copy to both tmux buffer and system clipboard using xsel
      # This ensures clipboard works both in terminal and with GUI apps
      # Use helper script to dynamically get correct DISPLAY for long-running sessions
      bind-key -T copy-mode C-w send-keys -X copy-pipe-and-cancel "$HOME/.local/bin/tmux-clipboard-helper xsel -i -p && $HOME/.local/bin/tmux-clipboard-helper xsel -o -p | $HOME/.local/bin/tmux-clipboard-helper xsel -i -b"
      bind-key -T copy-mode M-w send-keys -X copy-pipe-and-cancel "$HOME/.local/bin/tmux-clipboard-helper xsel -i -p && $HOME/.local/bin/tmux-clipboard-helper xsel -o -p | $HOME/.local/bin/tmux-clipboard-helper xsel -i -b"
      bind-key -T copy-mode MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "$HOME/.local/bin/tmux-clipboard-helper xsel -i -p && $HOME/.local/bin/tmux-clipboard-helper xsel -o -p | $HOME/.local/bin/tmux-clipboard-helper xsel -i -b"

      # Paste binding (Emacs-style) - paste from system clipboard
      # Use helper script to ensure correct DISPLAY even in long-running sessions
      bind-key C-y run-shell "$HOME/.local/bin/tmux-clipboard-helper xsel -o -b | tmux load-buffer - ; tmux paste-buffer"
      # Also bind without prefix for convenience
      bind-key -n C-y run-shell "$HOME/.local/bin/tmux-clipboard-helper xsel -o -b | tmux load-buffer - ; tmux paste-buffer"

      # Legacy xsel/xclip configurations (kept for reference)
      # If OSC 52 doesn't work in your terminal, uncomment one of these:

      # xsel (most reliable for tmux):
      # bind-key -T copy-mode C-w send-keys -X copy-pipe-and-cancel "DISPLAY=\${"DISPLAY:-:0"} xsel -i -p && DISPLAY=\${"DISPLAY:-:0"} xsel -o -p | DISPLAY=\${"DISPLAY:-:0"} xsel -i -b"
      # bind-key -T copy-mode M-w send-keys -X copy-pipe-and-cancel "DISPLAY=\${"DISPLAY:-:0"} xsel -i -p && DISPLAY=\${"DISPLAY:-:0"} xsel -o -p | DISPLAY=\${"DISPLAY:-:0"} xsel -i -b"
      # bind-key -T copy-mode MouseDragEnd1Pane send -X copy-pipe-and-cancel "DISPLAY=\${"DISPLAY:-:0"} xsel -i -p && DISPLAY=\${"DISPLAY:-:0"} xsel -o -p | DISPLAY=\${"DISPLAY:-:0"} xsel -i -b"

      # xclip (has STDOUT issues):
      # bind-key -T copy-mode C-w send-keys -X copy-pipe-and-cancel "xclip -i -selection clipboard > /dev/null"

      # Update environment variables when attaching
      set -g update-environment "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"

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
