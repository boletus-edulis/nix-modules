{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set fish_greeting # Disable greeting
    '';
    plugins = [
      { name = "grc"; src = pkgs.fishPlugins.grc.src; }
      { name = "done"; src = pkgs.fishPlugins.done.src; }
      { name = "spark"; src = pkgs.fishPlugins.spark.src; }
      { name = "hydro"; src = pkgs.fishPlugins.hydro.src; }
    ];
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
      if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
      then
        shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
        exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
      fi
    '';
  };

  programs.emacs = {
    enable = true;
    #package = pkgs.emacs-git-nox;
    #package = pkgs.emacs-nox;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      epkgs.vterm epkgs.treesit-grammars.with-all-grammars epkgs.use-package
      epkgs.helm epkgs.treesit-auto epkgs.doom-modeline epkgs.magit
      epkgs.blacken epkgs.flycheck epkgs.yasnippet epkgs.nix-ts-mode
      epkgs.yaml-mode epkgs.yasnippet-capf epkgs.nerd-icons
      epkgs.nerd-icons-completion epkgs.nerd-icons-grep
    ];
    extraConfig = (builtins.readFile ./init.el);
  };

  #home.file.emacs-init-el = {
  #  enable = true;
  #  target = ".emacs.d/init.el";
  #  source = ./init.el;
  #};

  home.packages = with pkgs; [ ispell hunspell iosevka-bin ];
}
