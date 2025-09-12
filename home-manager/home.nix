{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "hienphamduc";
  home.homeDirectory = "/home/hienphamduc";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  nixpkgs.config.allowUnfree = true;

  # gnome
  dconf.settings = {
    "org/gnome/desktop/peripherals/mouse" = {
      natural-scroll = true;
    };
    "org/gnome/desktop/peripherals/touchpad" = {
      natural-scroll = true;
    };
    "org/gnome/mutter/keybindings" = {
      toggle-tiled-left = ["<Super><Shift>h"];
      toggle-tiled-right = ["<Super><Shift>l"];
    };
    "org/gnome/desktop/wm/keybindings" = {
      close = ["<Super>c"];

      toggle-fullscreen = ["<Super><Shift>f"];
      toggle-maximized = ["<Super><Shift>m"];

      switch-windows = ["<Super>d"];
      activate-window-menu = [];

      switch-to-workspace-1 = ["<Super>1"];
      move-to-workspace-1 = ["<Super><Shift>1"];
      switch-to-workspace-2 = ["<Super>2"];
      move-to-workspace-2 = ["<Super><Shift>2"];
      switch-to-workspace-3 = ["<Super>3"];
      move-to-workspace-3 = ["<Super><Shift>3"];
      switch-to-workspace-4 = ["<Super>4"];
      move-to-workspace-4 = ["<Super><Shift>4"];
    };
  };

  home.packages = with pkgs; [
    emacs
    vim
    neovim
    git
    ripgrep
    fd
    wget
    curl
    rsync
    cmake
    gnumake
    openssl
    openssh

    # Development
    nodejs_22
    yarn
    prisma-engines
    nodePackages.prettier
    typescript-language-server
    eslint
    docker-compose
    yq
    gh
    tokei

    go
    gopls

    shellcheck

    brave
    # flatpak
    # gnome.gnome-software
    logseq

    # Editor
    vscode
    code-cursor

    # AI-assisted Programming
    claude-code
    opencode

    sqlite

    # TUIs
    lazygit
    lazydocker

    # API Client
    postman

    # DB Client
    dbeaver-bin

    # nerdfonts

    # terminal
    wezterm
    ghostty
    btop

    fastfetch

    # dependencies for emacs vterm
    libtool
    libvterm

    # dependencies for zsh cfg
    autojump
    fzf
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-powerlevel10k
    zsh-autocomplete

    htop

    arandr
    pavucontrol
    # ? networkmanagerapplet
    # ? blueman
    xfce.xfce4-power-manager
    lxappearance

    multimarkdown

    #i3
    rofi
    polybar

    # hyprland

    # Terminal
    kitty

    # Applicaiton Launcher
    wofi

    # File Manager
    kdePackages.dolphin
    yazi

    # Status Bar
    waybar

    # wallpaper
    hyprpaper

    # Bluetooth Manager
    blueman

    # Screen Lock
    hyprlock

    # key custom
    keyd

    wl-clipboard

    # wip: dependencies for bash cfg
    # shell prompt configuration
    # starship
    # line editor for bash
    # blesh

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".emacs.d/early-init.el".source = ./dotfiles/emacs/early-init.el;
    ".emacs.d/init.el".source = ./dotfiles/emacs/init.el;
    ".emacs.d/lisp".source = ./dotfiles/emacs/lisp;
    ".emacs.d/straight/versions/default.el".source = ./dotfiles/emacs/straight.lock.el;
    ".p10k.zsh".source = ./dotfiles/zsh/p10k.zsh;

    ".wezterm.lua".source = ./dotfiles/wezterm.lua;

    ".ssh/config".source = ./dotfiles/ssh/config;
    ".gitconfig".source = ./dotfiles/gitconfig;
    ".config/i3/config".source = ./dotfiles/i3/config;
    ".config/ghostty/config".source = ./dotfiles/ghostty/config;
    ".config/polybar/config.ini".source = ./dotfiles/polybar/config.ini;

    ".config/waybar/config.jsonc".source = ./dotfiles/waybar/config.jsonc;
    ".config/waybar/style.css".source = ./dotfiles/waybar/style.css;

    ".config/hypr/hyprland.conf".source = ./dotfiles/hyprland/hyprland.conf;
    ".config/hypr/hyprpaper.conf".source = ./dotfiles/hyprland/hyprpaper.conf;
    ".config/hypr/wallpapers/fedora36-1-cut.png".source = ./dotfiles/hyprland/wallpapers/fedora36-1-cut.png;
    ".config/hypr/hyprlock.conf".source = ./dotfiles/hyprland/hyprlock.conf;

    ".local/scripts/tmux/tmux-sessionizer".source = ./dotfiles/tmux/tmux-sessionizer;
  };

  home.sessionVariables = {
    # EDITOR = "emacs";

    # Set environment variables for input method integration
    # GTK_IM_MODULE = "ibus";
    # QT_IM_MODULE = "ibus";
    # XMODIFIERS = "@im=ibus";
  };

  programs.info.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs = {
    zsh = {
      enable = true;
      enableCompletion = false;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;

      shellAliases = {
        ls = "ls --color=auto";
        v = "nvim";
        gau = "git add -u";
        gaa = "git add -A";
        gb = "git branch";
        gci = "git commit";
        gpl = "git pull origin $(git branch --show-current)";
        gps = "git push origin $(git branch --show-current)";
        gsw = "git switch";
        gl = "git log --oneline --graph";
        glh = "git log --pretty=oneline --graph";
        gs = "git status";
        tmuxs = "$HOME/.local/scripts/tmux/tmux-sessionizer";
      };

      initExtra = ''
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
        [[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

        source ${pkgs.zsh-autocomplete}/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh

        export FZF_DEFAULT_OPTS='--height 60% --layout=reverse --border'
        export NIX_SHELL_PRESERVE_PROMPT="1";

        export PRISMA_QUERY_ENGINE_LIBRARY="${pkgs.prisma-engines}/lib/libquery_engine.node";
        export PRISMA_QUERY_ENGINE_BINARY="${pkgs.prisma-engines}/bin/query-engine";
        export PRISMA_SCHEMA_ENGINE_BINARY="${pkgs.prisma-engines}/bin/schema-engine";
        export PRISMA_FMT_BINARY="${pkgs.prisma-engines}/bin/prisma-fmt";

      '';

      oh-my-zsh = {
        enable = true;
        plugins = [
          "autojump"
          "vi-mode"
          "fzf"
        ];
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };

    # wip
    # bash = {
    #   enable = true;
    #   shellAliases = {
    #     ls = "ls --color=auto";
    #     v = "nvim";
    #     gau = "git add -u";
    #     gaa = "git add -A";
    #     gb = "git branch";
    #     gci = "git commit";
    #     gpl = "git pull origin $(git branch --show-current)";
    #     gps = "git push origin $(git branch --show-current)";
    #     gsw = "git switch";
    #     gl = "git log --oneline --graph";
    #     glh = "git log --pretty=oneline --graph";
    #     gs = "git status";
    #   };
    #   initExtra = ''
    #     export FZF_DEFAULT_OPTS='--height 60% --layout=reverse --border'
    #     # eval "$(starship init bash)"
    #     source ${pkgs.blesh}/share/blesh/ble.sh
    #   '';
    # };

    tmux = {
      enable = true;
      plugins = with pkgs.tmuxPlugins; [
        sensible
        {
          plugin = resurrect;
          extraConfig = ''
            set -g @resurrect-save 'S'
            set -g @resurrect-restore 'R'
          '';
        }
        prefix-highlight
      ];
      extraConfig = ''
# unbind default prefix and set it to <C-Space>
# unbind C-b
# set -g prefix C-Space
# bind C-Space send-prefix

# Terminal colors

# Set to use 256 colors
# set -g default-terminal "screen-256color"
set-option -ga terminal-overrides ',xterm-256color:Tc' # to support WezTerm with True color

# for Nix system
set-option -g default-shell /run/current-system/sw/bin/zsh
# for regular system
# set-option -g default-shell /bin/zsh

is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
    | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?)(diff)?$'"

is_emacs='tmux display-message -p "#{pane_current_command}" | grep -q emacs'

bind -n C-h  if-shell  "$is_vim"  "send-keys C-h"  "select-pane -L"
bind -n C-j   if-shell  "$is_vim"  "send-keys C-j"   "select-pane -D"
bind -n C-k  if-shell  "$is_vim"  "send-keys C-k"  "select-pane -U"
bind -n C-l   if-shell  "$is_vim"  "send-keys C-l"   "select-pane -R"
bind -n C-\   if-shell  "$is_vim"  "send-keys C-\\"  "select-pane -l"

# bind -n C-h  if-shell  "$is_emacs"  "send-keys C-h"  "select-pane -L"
# bind -n C-j   if-shell  "$is_emacs"  "send-keys C-j"   "select-pane -D"
# bind -n C-k  if-shell  "$is_emacs"  "send-keys C-k"  "select-pane -U"
# bind -n C-l   if-shell  "$is_emacs"  "send-keys C-l"   "select-pane -R"
# bind -n C-\   if-shell  "$is_emacs"  "send-keys C-\\"  "select-pane -l"

# for running tmux inside Emacs vterm
# bind h select-pane -L
# bind j select-pane -D
# bind k select-pane -U
# bind l select-pane -R

set -g mouse on
# set vi mode for copy mode
setw -g mode-keys vi
# more settings to make copy-mode more vim-like
unbind p
bind p paste-buffer

# Default keybinding to get into copy mode is prefix+[.
bind -Tcopy-mode-vi v send-keys -X begin-selection
bind -Tcopy-mode-vi C-v send-keys -X rectangle-toggle
bind -Tcopy-mode-vi y send-keys -X copy-selection-and-cancel

# linux
# bind -Tcopy-mode-vi 'y' send -X copy-pipe 'xclip -in -selection clipboard'
# bind -Tcopy-mode-vi C-c send -X copy-pipe 'xclip -in -selection clipboard'

# bind -Tcopy-mode-vi MouseDragEnd1Pane send -X copy-pipe-and-cancel 'xclip -in -selection clipboard'
# unbind -Tcopy-mode-vi MouseDragEnd1Pane

#### COPY PASTE WITH MOUSE ####
# Hold Shilft and use mouse to select text -> Still hold shift and use mouse right clict to select Copy option.

# Reload config
bind r source-file ~/.tmux.conf \; display "Config reloeaded"

# resize-pane
bind m resize-pane -Z # fullscreen
bind > resize-pane -R 12
bind < resize-pane -L 12
bind + resize-pane -U 12
bind - resize-pane -D 12

bind i split-window -v -c '#{pane_current_path}'
bind v split-window -h -c '#{pane_current_path}'

bind x kill-pane

bind -r p run-shell "tmux neww $HOME/.local/scripts/tmux/tmux-sessionizer"

# switch to the last session
bind s switch-client -l

# switch to the last window
bind w last-window

# Start windows and panes at 1, not 0
set -g base-index 1
setw -g pane-base-index 1


# copy mode
setw -g mode-style 'fg=black bg=white'

# pane
set -g pane-border-style bg=default,fg=black
set -g pane-active-border-style bg=default,fg=red
## set inactive/active window styles
set -g window-style fg=default,bg=#292a3b
set -g window-active-style fg=default,bg=#1e1e2e

## window
setw -g window-status-style 'fg=white bg=black'
setw -g window-status-current-style 'fg=white bg=black'
setw -g window-status-format '#[fg=white]#I:#W'
setw -g window-status-current-format '#[fg=yellow]#I:#W'
setw -g window-status-bell-style 'fg=yellow bg=red bold'

## messages
set -g message-style 'fg=black bg=green'

# statusbar
set -g status-position top
set -g status-justify left
set -g status-style fg=white,bg=black

set -g status-left "#[bold]#[fg=green]|#{session_name}| "
## Set the maximum length of the left component of the status bar. The default
# is 10.
set -g status-left-length 80

MYNAME=hienpd
# time format : https://linux.die.net/man/3/strftime
set -g status-right "#{prefix_highlight} $MYNAME | %R | %F "
# set -g status-right-style 'fg=white bg=black'
set -g status-right-length 50

# Hit prefix + I (capital I) to fetch the plugin and source it.
# List of plugins
# I'm using Nix to manage these plugins instead.
# set -g @plugin 'tmux-plugins/tpm'
# set -g @plugin 'tmux-plugins/tmux-sensible'

# # Session save/restore
# set -g @plugin 'tmux-plugins/tmux-resurrect'
# set -g @resurrect-save 'S'
# set -g @resurrect-restore 'R'

# set -g @plugin 'sainnhe/tmux-fzf'
# TMUX_FZF_LAUNCH_KEY="t"

# set -g @plugin 'tmux-plugins/tmux-prefix-highlight'

set -g set-clipboard on

# Initialize TMUX plugin manager (keep this line at the very bottom of tmux.conf)
# run -b '~/.tmux/plugins/tpm/tpm'
      '';
    };

  };

  services = {
    # Clipboard Manager
    copyq = {
      enable = true;
    };

    emacs = {
      enable = true;
      startWithUserSession = "graphical";
    };
  };

  systemd.user.services = {
    spice-vdagent = {
      Unit = {
        Description = "Spice guest session agent";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
        ConditionVirtual = "vm";
      };
      Service = {
        ExecStart = "${pkgs.spice-vdagent}/bin/spice-vdagent -x";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    emacsclient = {
      Unit = {
        Description = "Launch emacsclient GUI after emacs daemon";

        # after specifies the order in which units are started or stopped.
        # the current unit will be started after the units listed in after.
        After = [ "emacs.service" ];
      };
      Service = {
        ExecStart = "${pkgs.emacs}/bin/emacsclient -c";
        Restart = "no";
      };
      Install = {
        # wantedBy declares this unit as a dependency target to start when enabling another unit.
        WantedBy = [ "emacs.service" ];
      };
    };
  };
}
