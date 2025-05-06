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
  home.stateVersion = "24.11"; # Please read the comment before changing.

  nixpkgs.config.allowUnfree = true;

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
    shellcheck

    brave
    # flatpak
    # gnome.gnome-software
    vscode

    sqlite

    # nerdfonts

    wezterm

    # dependencies for emacs vterm
    libtool
    libvterm

    # dependencies for zsh cfg
    autojump
    fzf
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-powerlevel10k

    htop

    arandr
    pavucontrol
    # ? networkmanagerapplet
    # ? blueman
    xfce.xfce4-power-manager
    lxappearance

    # wip: dependencies for bash cfg
    # shell prompt configuration
    # starship
    # line editor for bash
    # blesh

    # logseq # does not available for aarch64-linux

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

  };

  home.sessionVariables = {
    # EDITOR = "emacs";
    NIX_SHELL_PRESERVE_PROMPT = "1";
  };

  programs.info.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
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

        export FZF_DEFAULT_OPTS='--height 60% --layout=reverse --border'
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
