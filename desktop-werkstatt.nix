{ pkgs, cpkgs, config, username, ... }:

{
  options = {};
  config = {
    services.cinnamon.apps.enable = true;

    services.xserver = {
      enable = true;
      desktopManager = {
        cinnamon.enable = true;
        xterm.enable = false;
      };

      displayManager.lightdm = {
        enable = true;
      };
    };

    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        swtpm.enable = true;
        vhostUserPackages = [ pkgs.virtiofsd ];
      };
    };

  };
}
