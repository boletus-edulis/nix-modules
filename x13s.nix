{ inputs, pkgs, cpkgs, stdenv, lib, config, modulesPath, options, username, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.binfmt.emulatedSystems = [
    "x86_64-linux"
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    useOSProber = true;
    device = "nodev";
    extraPerEntryConfig = "devicetree ${builtins.toString cpkgs.linux_x13s_6_10}/dtbs/qcom/sc8280xp-lenovo-thinkpad-x13s.dtb";
  };
  boot.kernelPackages = pkgs.linuxPackagesFor cpkgs.linux_x13s_6_10;
  boot.kernelParams = [
    "earlyprintk=efi" "loglevel=7" "console=tty0"
    "clk_ignore_unused" "pd_ignore_unused" "firmware_class.path=${
      builtins.toString cpkgs.x13s-firmware
    }/lib/firmware" "arm64.nopauth"
  ];

  boot.initrd = let
    modules = [ "nvme" "phy_qcom_qmp_pcie" "pcie_qcom" "phy_qcom_qmp_ufs" "ufs_qcom" "i2c_hid_of"
                "i2c_qcom_geni" "leds_qcom_lpg" "pwm_bl" "qrtr" "pmic_glink_altmode" "gpio_sbu_mux"
                "phy_qcom_qmp_combo" "gpucc_sc8280xp" "dispcc_sc8280xp" "phy_qcom_edp" "panel_edp"
                "msm" ];
  in {
    availableKernelModules = modules;
    kernelModules = modules;
    verbose = true;
    supportedFilesystems = { btrfs = true; };
  };

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

  services.udev.extraRules = ''
    # wifi mac address
    ACTION=="add", SUBSYSTEM=="net", KERNELS=="0006:01:00.0", RUN+="${pkgs.iproute2}/bin/ip link set dev $name address 00:00:af:1e:ee:ef"
    # libcamera access to dma_heaps unresolved
    ACTION=="add", SUBSYSTEM=="dma_heap", KERNEL=="linux,cma", GROUP="video", MODE="0660"
    ACTION=="add", SUBSYSTEM=="dma_heap", KERNEL=="system", GROUP="video", MODE="0660"
  '';

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

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  hardware.enableAllFirmware = lib.mkForce true;
  hardware.firmware = lib.mkForce [
    cpkgs.x13s-firmware
  ];

  hardware.graphics.enable = true;
  hardware.graphics.extraPackages = with pkgs; [
    vaapiVdpau
    libvdpau-va-gl
  ];

  home-manager.users."${username}" = { pkgs, config, ... }: {
    home.sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = 1;
    };
  };
}
