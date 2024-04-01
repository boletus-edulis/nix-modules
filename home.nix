{ pkgs, config, username, ... }:

{
  options = {};
  config = {
    users.users."${username}" = {
      isNormalUser = true;
      extraGroups = [ "wheel" "libvirtd" "podman" "video" ];
    };

    home-manager.users."${username}" = { pkgs, config, ... }: {
      programs.bash.enable = true;
      programs.nushell = {
        enable = true;
      };
      home.packages = with pkgs; [
        atool curl git conntrack-tools lsof file dnsutils tmux efibootmgr
      ];
      home.stateVersion = "23.11";
      programs.emacs = {
        enable = true;
        package = pkgs.emacs-nox;
        extraPackages = epkgs: [ epkgs.vterm ];
      };
      services.emacs = {
        enable = true;
        defaultEditor = true;
        startWithUserSession = true;
        client.arguments = [ "-nw" ];
      };
      home.file.emacs-init-el = {
        enable = true;
        target = ".emacs.d/init.el";
        source = ./init.el;
      };
    };
  };
}
