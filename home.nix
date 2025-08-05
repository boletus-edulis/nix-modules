{ lib, pkgs, config, username, inputs, ... }:

let
  emacs = import ./emacs.nix { inherit config pkgs; };
  home = (
    lib.recursiveUpdate
      {
        packages = with pkgs; [
          atool curl git conntrack-tools lsof file dnsutils tmux efibootmgr iotop
          nftables tcpdump gdb emacs-lsp-booster
        ];
        stateVersion = "23.11";
      }
      emacs.home
  );
in
{
  options = {};
  config = {
    #nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
    users.users."${username}" = {
      isNormalUser = true;
      extraGroups = [ "networkmanager" "wheel" "libvirtd" "podman" "video" "rtkit" ];
    };

    home-manager.backupFileExtension = "backup";
    home-manager.users."${username}" = { pkgs, config, ... }: {
      #nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
      inherit home;
      inherit (emacs) programs;
    };
  };
}
