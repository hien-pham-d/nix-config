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

    nodejs_22
    yarn
    prisma-engines
    nodePackages.prettier
    typescript-language-server
    eslint
    docker-compose
    yq

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

    ".tmux.conf".source = ./dotfiles/tmux.conf;
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
    };

  };

  services = {
    # Clipboard Manager
    copyq = {
      enable = true;
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
  };
}
