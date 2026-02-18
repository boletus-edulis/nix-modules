{ config, pkgs, ... }:

{
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
  };

  programs.emacs = {
    enable = true;
    #package = pkgs.emacs-git-nox;
    #package = pkgs.emacs-nox;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      epkgs.vterm epkgs.treesit-grammars.with-all-grammars
      epkgs.treemacs epkgs.treemacs-all-the-icons
    ];
    extraConfig = builtins.readFile ./init.el;
  };

  #home.file.emacs-init-el = {
  #  enable = true;
  #  target = ".emacs.d/init.el";
  #  source = ./init.el;
  #};

  home.packages = with pkgs; [ ispell hunspell ];
}
