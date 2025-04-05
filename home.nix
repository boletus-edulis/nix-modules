{ pkgs, config, username, inputs, ... }:

{
  options = {};
  config = {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
    users.users."${username}" = {
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" "libvirtd" "podman" "video" "rtkit" ];
    };

    home-manager.users."${username}" = { pkgs, config, ... }: {
      nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
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
      home.packages = with pkgs; [
        atool curl git conntrack-tools lsof file dnsutils tmux efibootmgr iotop
        nftables tcpdump gdb emacs-lsp-booster ispell hunspell
      ];
      home.stateVersion = "23.11";
      programs.emacs = {
        enable = true;
        package = pkgs.emacs-git-nox;
        extraPackages = epkgs: [
          epkgs.vterm epkgs.treesit-grammars.with-all-grammars
        ];
      };
      home.file.emacs-init-el = {
        enable = true;
        target = ".emacs.d/init.el";
        source = ./init.el;
      };
    };
  };
}
