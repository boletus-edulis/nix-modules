{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption mkIf types;

  cfg = config.services.simple-router;
in
{
  options = {
    services.simple-router = {
      enable = mkEnableOption "Unbound domain name server";

      externalInterface = mkOption {
        type = types.str;
        description = "Interface connected to the upstream network.";
      };

      internalInterface = mkOption {
        type = types.str;
        description = "Interface connected to the downstream network.";
      };
    };
  };

  config = mkIf cfg.enable rec {
    systemd.network.netdevs."10-simple-router-internal" = {
      netdevConfig = {
        Kind = "bridge";
        Name = "simple_router_bridge0";
      };
    };

    systemd.network.networks."10-simple-router-internal" = {
      matchConfig.Name = systemd.network.netdevs."10-simple-router-internal".netdevConfig.Name;
      addresses = [
        { Address = "172.16.0.1/24"; }
      ];
    };

    systemd.network.networks."10-simple-router-nic" = {
      networkConfig.Bridge = systemd.network.netdevs."10-simple-router-internal".netdevConfig.Name;
      matchConfig.Name = "${cfg.internalInterface}";
    };

    networking.nat.enable = true;
    networking.nat.externalInterface = "${cfg.externalInterface}";
    networking.nat.internalInterfaces = [
      systemd.network.netdevs."10-simple-router-internal".netdevConfig.Name
    ];

    services.kea = {
      dhcp4 = {
        enable = true;
        extraArgs = [ "-d" ];
        settings = {
          interfaces-config = {
            interfaces = [ systemd.network.netdevs."10-simple-router-internal".netdevConfig.Name ];
          };
          lease-db = {
            name = "/var/lib/kea/dhcp4.leases";
            persist = true;
            type = "memfile";
          };
          option-data = [
            {
              name = "domain-name-servers";
              space = "dhcp4";
              data = "172.16.0.1";
            }
            {
              name = "routers";
              space = "dhcp4";
              data = "172.16.0.1";
            }
          ];
          subnet4 = [
            {
              id = 1;
              subnet = "172.16.0.1/24";
              pools = [
                {
                  pool = "172.16.0.100 - 172.16.0.200";
                }
              ];
            }
          ];
        };
      };
    };

    services.unbound = {
      enable = true;
      settings = {
        remote-control.control-enable = true;
        server = {
          interface = [ systemd.network.netdevs."10-simple-router-internal".netdevConfig.Name ];
          verbosity = 2;
        };
        forward-zone = [
          {
            name = ".";
            forward-addr = [
              "1.1.1.1@853#cloudflare-dns.com"
              "1.0.0.1@853#cloudflare-dns.com"
            ];
          }
        ];
      };
    };

  };
}
