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

    programs.dconf.enable = true;
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
    services.printing.enable = true;
    services.avahi.enable = true;
    # Important to resolve .local domains of printers, otherwise you get an error
    # like  "Impossible to connect to XXX.local: Name or service not known"
    services.avahi.nssmdns4 = true;

    services.syncthing = {
      enable = true;
      user = "Us0r";
      configDir = "/home/Us0r/.config/syncthing";
    };

    # virt-manager usb forwarding
    virtualisation.spiceUSBRedirection.enable = true;

    # home manager
    security.polkit.enable = true;

    # audio
    environment.systemPackages = with pkgs; [ alsa-ucm-conf ];
    services.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      extraConfig.pipewire = {
        "10-fix-crackling" = {
          "pulse.properties" = {
            "pulse.min.req" = "1024/48000";
            "pulse.min.frag" = "1024/48000";
            "pulse.min.quantum" = "1024/48000";
          };
        };
        "11-disable-suspend" = {
          "monitor.alsa.rules" = [
            {
              "matches" = [
                { # Matches all sources
                  "node.name" = "~alsa_input.*";
                }
                { # Matches all sinks
                  "node.name" = "~alsa_output.*";
                }
              ];
              "actions" = {
                "update-props" = {
                  "session.suspend-timeout-seconds" = 0;
#                 "api.alsa.headroom" = 1024;
#	                "api.alsa.period-size" = 128;
                };
              };
            }
          ];
        };
      };
    };

    fonts.packages = [
      pkgs.iosevka-bin
      #cpkgs.iosevka-term
      pkgs.font-awesome
    ];

    home-manager.users."${username}" = { pkgs, config, ... }: {
        home.packages = with pkgs; [
          swaylock helvum imhex flatpak virt-manager pavucontrol kdePackages.okular remmina
          thunderbird firefox signal-desktop mumble chromium kdePackages.skanlite jami
        ];
        home.pointerCursor = {
          name = "Adwaita";
          package = pkgs.adwaita-icon-theme;
          size = 24;
          gtk.enable = true;
          x11 = {
            enable = true;
            defaultCursor = "Adwaita";
          };
        };
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
        gtk.enable = true;
        programs.alacritty = {
          enable = true;
          settings = {
            #window = { # the cool ...
            #decorations = "None";
            #opacity = 0.7;
            #};
            colors = {
              "draw_bold_text_with_bright_colors" = false;
            };
            env = {
              TERM = "xterm-256color";
              COLORTERM = "true";
              #WINIT_X11_SCALE_FACTOR = "1.0";
            };
            scrolling = {
              history = 100000;
            };
            font = {
              size = 14;
              bold = {
                family = "Iosevka Term";
                style = "Bold";
              };
              bold_italic = {
                family = "Iosevka Term";
                style = "Bold Italic";
              };
              italic = {
                family = "Iosevka Term";
                style = "Italic";
              };
              normal = {
                family = "Iosevka Term";
                style = "Regular";
              };
            };
          };
        };
      };
  };
}
