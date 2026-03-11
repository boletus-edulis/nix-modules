{ config, pkgs, lib, ... }:

{
  programs.nushell = {
    enable = true;
    plugins = [
      pkgs.nushellPlugins.formats
      pkgs.nushellPlugins.query
      pkgs.nushellPlugins.polars
      pkgs.nushellPlugins.highlight
      pkgs.nushellPlugins.desktop_notifications
      pkgs.nushellPlugins.units
    ];
    settings = {
      show_banner = false;
      history = {
	format = "sqlite";
      };
    };
  };

  programs.bash = {
    enable = true;
    historySize = -1;
    historyFileSize = -1;
    bashrcExtra = let
      dicts = [
        pkgs.hunspellDicts.de-de
        pkgs.hunspellDicts.de_DE
        pkgs.hunspellDicts.en-us
        pkgs.hunspellDicts.en_US
        pkgs.hunspellDictsChromium.de-de
        pkgs.hunspellDictsChromium.de_DE
        pkgs.hunspellDictsChromium.en-us
        pkgs.hunspellDictsChromium.en_US
      ];
      buildPath = dict: "${dict}/share/hunspell";
    in ''
      export DICPATH=${builtins.concatStringsSep ":" (builtins.map buildPath dicts)}
    '';
    initExtra = ''
      if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "nu" && -z ''${BASH_EXECUTION_STRING} ]]
      then
        shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
        exec ${pkgs.nushell}/bin/nu $LOGIN_OPTION
      fi
    '';
  };

  programs.emacs = {
    enable = true;
    #package = pkgs.emacs-git-nox;
    #package = pkgs.emacs-nox;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      epkgs.vterm
      (epkgs.treesit-grammars.with-grammars (
	x: builtins.attrValues (lib.attrsets.filterAttrs (
	  n: v: if (builtins.toString n) == "tree-sitter-quint" then false else true)
	  x)))
      epkgs.use-package epkgs.nushell-ts-mode epkgs.powershell
      epkgs.helm epkgs.treesit-auto epkgs.doom-modeline epkgs.magit
      epkgs.blacken epkgs.flycheck epkgs.yasnippet epkgs.nix-ts-mode
      epkgs.yaml-mode epkgs.yasnippet-capf epkgs.nerd-icons
      epkgs.nerd-icons-grep epkgs.smartparens
    ];
    extraConfig = (builtins.readFile ./init.el);
  };

  #home.file.emacs-init-el = {
  #  enable = true;
  #  target = ".emacs.d/init.el";
  #  source = ./init.el;
  #};

  home.packages = with pkgs; [
    ispell hunspell iosevka-bin lsof file dnsutils tmux curl git tcpdump gdb
    strace
  ];
}
