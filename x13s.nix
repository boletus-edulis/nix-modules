{ inputs, pkgs, cpkgs, stdenv, lib, config, modulesPath, options, username, ... }:

let
  modules = [
    "qcom_stats"
    "rpm_master_stats"
    "qcom_tsens"
    "socinfo"
    "qcom-rpmh-regulator"
    "rpmhpd"
    "rpmpd"
    "qcom_q6v5_adsp"
    "phy-qcom-qmp-pcie-msm8996"
    "pinctrl-sc8280xp-lpass-lpi"
    "smem"
    "smsm"
    "nvme"
    "qcom-cpufreq-hw"
    "qcom-cpufreq-nvmem"
    "pcie_qcom"
    "spi-geni-qcom"
    "i2c-qcom-geni"
    "qcom_rpm"
    "qcom_wdt"
    "rpm-proc"
    "icc-smd-rpm"
    "qcom_glink_rpm"
    "qcom-rpmh-regulator"
    "qcom-rpm-regulator"
    "clk-rpm"
    "clk-smd-rpm"
    "qcom-coincell"
    "fastrpc"
    "btrfs"
  ];
in {
  imports =
    [ # Include the results of the hardware scan.
      (modulesPath + "/installer/scan/not-detected.nix")
      # inputs.nix13s.nixosModules.nix13s
    ];

  #  nixpkgs.overlays = [
  #    (final: super: {
  #      libvirt = super.libvirt.override(_: {
  #        enableZfs = false;
  #      });
  #      zfs = super.zfs.overrideAttrs(_: {
  #        meta.platforms = [];
  #      });
  #    })
  #  ];

  #  boot.binfmt.emulatedSystems = [
  #    "x86_64-linux"
  #  ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    useOSProber = true;
    device = "nodev";
    extraPerEntryConfig = "devicetree ${builtins.toString cpkgs.linux_x13s}/dtbs/qcom/sc8280xp-lenovo-thinkpad-x13s.dtb";
  };
  boot.kernelPackages = pkgs.linuxPackagesFor cpkgs.linux_x13s;
  boot.kernelParams = [
    "earlyprintk=efi" "loglevel=7" "console=tty0"
    "clk_ignore_unused" "firmware_class.path=${
      builtins.toString cpkgs.x13s-firmware
    }/lib/firmware"
  ];
  boot.initrd.availableKernelModules = [ "nvme" "btrfs" "dm_mod" ]; #modules;
  boot.initrd.kernelModules = [ "nvme" "btrfs" "dm_mod" ]; #modules;

  #programs.nushell.enable = true;
  #users.defaultUserShell = pkgs.nushell;

  console = {
    keyMap = "us";
  };

  environment.systemPackages = with pkgs; [ cpkgs.qrtr cpkgs.pd-mapper ];

  systemd.services.pd-mapper = {
    enable = true;
    description = "pd-mapper qualcom battery";
    serviceConfig = {
      ExecStart = "${cpkgs.pd-mapper}/bin/pd-mapper";
      User = "root";
      Type = "simple";
    };
    wantedBy = [ "multi-user.target" ];
  };

  virtualisation.libvirtd.enable = true;
  virtualisation.waydroid.enable = true;

  virtualisation.podman.enable = true;
  virtualisation.podman.autoPrune.enable = true;

  services.tlp = {
    enable = true;
    settings = {
      RUNTIME_PM_ON_AC = "on";
      RUNTIME_PM_ON_BAT = "auto";
      USB_AUTOSUSPEND = 1;
    };
  };

  networking.hostName = "ribes-uva-crispa";
  networking.networkmanager.enable = true;
  networking.nftables.enable = true;
  networking.firewall.allowedTCPPorts = [ 22000 ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It’s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/e1c8ea9d-82b1-4e5a-a0f1-be1a5d099160";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" "discard" ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/FE80-11C1";
      fsType = "vfat";
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  #networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlP6p1s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  hardware.enableAllFirmware = lib.mkForce true;
  hardware.firmware = lib.mkForce [
    cpkgs.x13s-firmware
  ];

  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiVdpau
    libvdpau-va-gl
  ];
}
