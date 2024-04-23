{ pkgs, cpkgs, config, username, ... }:

{
  options = {};
  config = {
    programs.dconf.enable = true;
    programs.xwayland.enable = true;
    # udev rules for light
    programs.light.enable = true;

    services.xserver.enable = true;

    services.greetd = let
      swayConfig = pkgs.writeText "greetd-sway-config" ''
       # `-l` activates layer-shell mode. Notice that `swaymsg exit` will run after gtkgreet.
       exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -s ${config.environment.etc."greetd/gtkgreet.css".source}; swaymsg exit"
       bindsym Mod4+shift+e exec swaynag \
         -t warning \
         -m 'What do you want to do?' \
         -b 'Poweroff' 'systemctl poweroff' \
         -b 'Reboot' 'systemctl reboot'
       '';
    in {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.sway}/bin/sway --config ${swayConfig}";
        };
      };
    };

    environment.etc = {
      "greetd/environments".text = ''
        sway
      '';
      "greetd/gtkgreet.css".text = ''
        window {
          /*background-image: url("file:///usr/share/backgrounds/default.png");*/
          background-size: cover;
          /*background-position: center;*/
          background-color: rgba(30, 30, 30, 1.0);
        }

        box#body {
          background-color: rgba(50, 50, 50, 0.5);
          border-radius: 10px;
          padding: 50px;
        }

        clock_label {
          background-color: rgba(250, 250, 250, 0.5);
        }
      '';
    };

    #services.xserver.displayManager.sddm.enable = true;
    #services.xserver.displayManager.sddm.wayland.enable = true;
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
    security.rtkit.enable = true;

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-wlr
        pkgs.xdg-desktop-portal-kde
        pkgs.xdg-desktop-portal-gtk
      ];
      wlr.enable = true;
      configPackages = [ pkgs.gnome.gnome-session ];
    };

    # Enable sound with pipewire.
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

    fonts.packages = [
      pkgs.iosevka
      cpkgs.iosevka-term
      pkgs.font-awesome
    ];

    home-manager.users."${username}" = { pkgs, config, ... }: {
      home.packages = with pkgs; [
        swaylock helvum imhex flatpak virt-manager pavucontrol okular
        thunderbird firefox signal-desktop mumble chromium
      ];
      home.pointerCursor = {
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
        size = 24;
        gtk.enable = true;
        x11 = {
          enable = true;
          defaultCursor = "Adwaita";
        };
      };
      home.sessionVariables = {
        QT_QPA_PLATFORM = "wayland";
        GDK_BACKEND = "wayland";
        MOZ_ENABLE_WAYLAND = 1;
      };
      home.keyboard = {
        layout = "us";
        variant = "altgr-intl";
      };
      gtk.enable = true;
      programs.alacritty = {
        enable = true;
        settings = {
          colors =  {
            "draw_bold_text_with_bright_colors" = true;
          };
          env = {
            TERM = "xterm-256color";
            #WINIT_X11_SCALE_FACTOR = "1.0";
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
      programs.waybar = {
        enable = true;
        settings = {
          mainBar = {
            layer = "top";
            position = "top";
            height = 30;

            modules-left = [
              "cpu" "memory" "sway/workspaces" "sway/mode" "sway/scratchpad"
            ];
            modules-center = [
              "sway/window"
            ];
            modules-right = [
              "idle_inhibitor" "network" "temperature" "backlight" "battery"
              "wireplumber" "clock" "tray"
            ];

            "cpu" = {
              interval = 2;
              format = "{usage}% ";
              max-length = 10;
            };

            "memory" = {
              interval = 2;
              format = "{}% ";
              max-length = 10;
            };

            "wireplumber" = {
              format = "{volume}% {icon}";
              format-muted = "";
              on-click = "helvum";
              format-icons = [ "" "" "" ];
            };

            "idle_inhibitor" = {
              format = "{icon}";
              format-icons = {
                activated = "";
                deactivated = "";
              };
            };

            "backlight" = {
              format = "{percent}% {icon}";
              format-icons = [ "" ];
            };

            "battery" = {
              interval = 60;
              states = {
                warning = 30;
                critical = 15;
              };
              format = "{capacity}% {icon}";
              format-icons = [ "" "" "" "" "" ];
              max-length = 25;
            };

            "temperature" = {
              critical-threshold = 80;
              format-critical = "{temperatureC}°C ";
              format = "{temperatureC}°C ";
            };

            "clock" = {
              interval = 5;
              tooltip = true;
              format = "{:%H:%M:%S} ";
              tooltip-format = "{:%Y-%m-%d}";
            };
          };
        };
      };
      wayland.windowManager.sway = rec {
        enable = true;
        config = {
          modifier = "Mod4";

          down = "n";
          up = "p";
          left = "b";
          right = "f";

          terminal = "alacritty";

          startup = [
            { command = "waybar"; }
          ];

          focus.followMouse = "no";
          fonts = {
            names = [ "Iosevka Term" ];
            style = "Regular";
            size = 12.0;
          };

          input = {
            "*" = {
              xkb_variant = "altgr-intl";
              xkb_layout = "us";
              events = "enabled";
            };
          };

          seat = {
            "*" = {
              keyboard_grouping = "none";
            };
          };

          keybindings = let
            inherit (config)
              modifier terminal left down up right;
          in
            {
              "${modifier}+l" = "exec ${pkgs.swaylock}/bin/swaylock -fF -u -c 000000";
              "${modifier}+Return" = "exec ${terminal}";
              "${modifier}+Shift+q" = "kill";
              "${modifier}+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";

              "${modifier}+${left}" = "focus left";
              "${modifier}+${down}" = "focus down";
              "${modifier}+${up}" = "focus up";
              "${modifier}+${right}" = "focus right";

              "${modifier}+Left" = "focus left";
              "${modifier}+Down" = "focus down";
              "${modifier}+Up" = "focus up";
              "${modifier}+Right" = "focus right";

              "${modifier}+Shift+${left}" = "move left";
              "${modifier}+Shift+${down}" = "move down";
              "${modifier}+Shift+${up}" = "move up";
              "${modifier}+Shift+${right}" = "move right";

              "${modifier}+Shift+Left" = "move left";
              "${modifier}+Shift+Down" = "move down";
              "${modifier}+Shift+Up" = "move up";
              "${modifier}+Shift+Right" = "move right";

              "${modifier}+h" = "splith";
              "${modifier}+v" = "splitv";
              "${modifier}+f11" = "fullscreen toggle";
              "${modifier}+a" = "focus parent";

              "${modifier}+s" = "layout stacking";
              "${modifier}+w" = "layout tabbed";
              "${modifier}+e" = "layout toggle split";

              "${modifier}+Shift+space" = "floating toggle";
              "${modifier}+space" = "focus mode_toggle";

              "${modifier}+1" = "workspace number 1";
              "${modifier}+2" = "workspace number 2";
              "${modifier}+3" = "workspace number 3";
              "${modifier}+4" = "workspace number 4";
              "${modifier}+5" = "workspace number 5";
              "${modifier}+6" = "workspace number 6";
              "${modifier}+7" = "workspace number 7";
              "${modifier}+8" = "workspace number 8";
              "${modifier}+9" = "workspace number 9";
              "${modifier}+0" = "workspace number 10";

              "${modifier}+Shift+1" =
                "move container to workspace number 1";
              "${modifier}+Shift+2" =
                "move container to workspace number 2";
              "${modifier}+Shift+3" =
                "move container to workspace number 3";
              "${modifier}+Shift+4" =
                "move container to workspace number 4";
              "${modifier}+Shift+5" =
                "move container to workspace number 5";
              "${modifier}+Shift+6" =
                "move container to workspace number 6";
              "${modifier}+Shift+7" =
                "move container to workspace number 7";
              "${modifier}+Shift+8" =
                "move container to workspace number 8";
              "${modifier}+Shift+9" =
                "move container to workspace number 9";
              "${modifier}+Shift+0" =
                "move container to workspace number 10";

              "${modifier}+Shift+minus" = "move scratchpad";
              "${modifier}+minus" = "scratchpad show";

              "${modifier}+Shift+c" = "reload";
              "${modifier}+Shift+e" =
                "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";

              "${modifier}+r" = "mode resize";

              # brightness
              "XF86MonBrightnessUp" = "exec light -A 2";
              "XF86MonBrightnessDown" = "exec light -U 2";
              "XF86AudioRaiseVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+";
              "XF86AudioLowerVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
              "XF86AudioMute" = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            };

          modes = {
            resize = {
              Escape = "mode default";
              n = "resize shrink width 10 px";
              p = "resize grow height 10 px";
              b = "resize shrink height 10 px";
              f = "resize grow width 10 px";
            };
          };
          bars = [];
        };
      };
      services.swayidle = {
        enable = true;
        timeouts = [
          { timeout = 300; command = "${pkgs.swaylock}/bin/swaylock -fF -u -c 000000"; }
          { timeout = 600; command = ''${pkgs.sway}/bin/swaymsg "output * power off"'';
            resumeCommand = ''${pkgs.sway}/bin/swaymsg "output * power on"''; }
          #{ timeout = 90; command = "${pkgs.systemd}/bin/systemctl suspend"; }
        ];
        events = [
          { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock -fF -u -c 000000"; }
        ];
      };
      services.mako = {
        enable = true;
      };
      #services.gammastep = {
      #  enable = true;
      #  longitude = "48.2403029";
      #  latitude = "11.660478";
      #};
    };
  };
}
