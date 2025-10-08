{ pkgs, cpkgs, config, username, ... }:

{
  options = {};
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        sbclPackages = prev.sbclPackages.overrideScope (self: super: {
          clx = super.clx.overrideAttrs {
            version = "0.7.6";
            src = prev.fetchzip {
              url = "https://github.com/sharplispers/clx/archive/refs/tags/0.7.6.tar.gz";
              sha256 = "1p3rp97rqsznawi62im7hzxjxfv1b46h2hzj568kpks7lkvg8ag2";
            };
          };
        });
      })
    ];

    services.xserver = {
      enable = true;
      desktopManager = {
        #cinnamon.enable = true;

        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = false;
          enableXfwm = false;
        };

      };

      windowManager.stumpwm.enable = true;
      displayManager.lightdm = {
        enable = true;
        #greeters = {
        #  gtk.enable = true;
        #  gtk.theme.name = "Adwaita-dark";
        #};
      };

    };
    #services.displayManager.defaultSession = "none+stumpwm";

    services.syncthing = {
      enable = true;
      user = "Us0r";
      configDir = "/home/Us0r/.config/syncthing";
    };

    home-manager.users."${username}" = { pkgs, config, ... }: {
        home.sessionVariables = {
          GDK_CORE_DEVICE_EVENTS = "1";
        };
        home.file.stumpwm-init-el = {
          enable = true;
          target = ".stumpwm.d/init.lisp";
          source = ./init.lisp;
        };
        home.keyboard = {
          xkb.layout = "us";
          xkb.variant = "alt-intl";
        };
      };
  };
}
