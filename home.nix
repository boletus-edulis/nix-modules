{ pkgs, config, username, inputs, ... }:

{
  options = {};
  config = {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
    users.users."${username}" = {
      isNormalUser = true;
      extraGroups = [ "wheel" "libvirtd" "podman" "video" "rtkit" ];
    };

    home-manager.users."${username}" = { pkgs, config, ... }: {
      nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
      programs.bash.enable = true;
      home.packages = with pkgs; [
        atool curl git conntrack-tools lsof file dnsutils tmux efibootmgr iotop
        nftables tcpdump gdb
      ];
      home.stateVersion = "23.11";
      programs.emacs = {
        enable = true;
        package = pkgs.emacs-git-nox;
        extraPackages = epkgs: [ epkgs.vterm ];
      };
      #services.emacs = {
      #  enable = true;
      #  defaultEditor = true;
      #  startWithUserSession = true;
      #  client.arguments = [ "-nw" ];
      #};
      home.file.emacs-init-el = {
        enable = true;
        target = ".emacs.d/init.el";
        source = ./init.el;
      };
    };
  };
}
