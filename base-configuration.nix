{ pkgs, config, username, ... }:

{
  options = {};
  config = {
    nix = {
      settings = {
        experimental-features = [ "nix-command" "flakes" ];
        auto-optimise-store = true;
        substituters = [
          "https://cache.useless-thing.net/"
        ];
        trusted-public-keys = [
          "cache.useless-thing.net:gifkyXgOeSVeFzqi4kVhjry2SmW4g0L6lnxCmrqZczg="
        ];
      };
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
    };

    time.timeZone = "Europe/Berlin";
    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_IDENTIFICATION = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "de_DE.UTF-8";
    };

    console = {
      keyMap = "us";
    };

    services.openssh.enable = true;
    services.logrotate.enable = true;
    services.fwupd.enable = true;

    documentation.dev.enable = true;
    documentation.man.generateCaches = true;

    environment.systemPackages = with pkgs; [ man-pages man-pages-posix ];
  };
}
