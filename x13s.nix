{ inputs, pkgs, cpkgs, stdenv, lib, config, modulesPath, options, username, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.binfmt.emulatedSystems = [
    "x86_64-linux"
    #"i686-linux"
    #"armv7l-linux"
  ];
  boot.binfmt.preferStaticEmulators = true;

  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  #specialisation.notel2.inheritParentConfig = true;
  #specialisation.notel2.configuration = {
  #  hardware.deviceTree.name = lib.mkForce "sc8280xp-lenovo-thinkpad-x13s.dtb";
  #};

  hardware.deviceTree.name = lib.mkForce "sc8280xp-lenovo-thinkpad-x13s.dtb";
  #hardware.deviceTree.name = "sc8280xp-lenovo-thinkpad-x13s-el2.dtb";
  #boot.loader.systemd-boot.extraFiles = {
  #  "EFI/systemd/drivers/slbounceaa64.efi" = "${cpkgs.slbounce}/slbounce.efi";
  #  "EFI/systemd/drivers/${cpkgs.launch.pname}" = "${cpkgs.launch}/test/${cpkgs.launch.pname}";
  #
  #  "slbounce.efi" = "${cpkgs.slbounce}/slbounce.efi";
  #  "sltest.efi" = "${cpkgs.slbounce}/sltest.efi";
  #  "${cpkgs.launch.pname}" = "${cpkgs.launch}/test/${cpkgs.launch.pname}";
  #};
  hardware.deviceTree.package = lib.mkForce "${builtins.toString pkgs.linuxPackages_latest.kernel}/dtbs/qcom";
  hardware.deviceTree.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.installDeviceTree = true;
  boot.loader.systemd-boot.edk2-uefi-shell.enable = true;
  boot.loader.systemd-boot.configurationLimit = 2;

  boot.supportedFilesystems = [ "iso9660" "udf" ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "earlyprintk=efi" "loglevel=7" "console=tty0"
    "clk_ignore_unused" "pd_ignore_unused" "arm64.nopauth"
  ];

  boot.blacklistedKernelModules = [
    "camcc_sc8280xp"
  ];

  boot.initrd = let
    modules = [
      "nvme"
      "phy_qcom_qmp_pcie"
      "phy_qcom_qmp_ufs"
      "pcie-qcom" # new

      "i2c-core" # new
      "i2c-hid" # new

      "ufs_qcom"
      "i2c_hid_of"

      "i2c_qcom_geni"
      "leds_qcom_lpg"
      "pwm_bl"
      "qrtr"

      "pmic_glink_altmode"
      "gpio_sbu_mux"
      "phy_qcom_qmp_combo"
      "gpucc_sc8280xp"
      "dispcc_sc8280xp"
      "phy_qcom_edp"
      "panel_edp"
      "msm"
    ];
  in {
    extraFirmwarePaths = [
      "qcom/sc8280xp/SC8280XP-LENOVO-X13S-tplg.bin.zst"
      "qcom/sc8280xp/LENOVO/21BX/qcvss8280.mbn.zst"
      "qcom/sc8280xp/LENOVO/21BX/qcslpi8280.mbn.zst"
      "qcom/sc8280xp/LENOVO/21BX/qcdxkmsuc8280.mbn.zst"
      "qcom/sc8280xp/LENOVO/21BX/qccdsp8280.mbn.zst"
      "qcom/sc8280xp/LENOVO/21BX/qcadsp8280.mbn.zst"

      "qcom/sc8280xp/LENOVO/21BX/cdspr.jsn.zst"
      "qcom/qcm6490/cdspr.jsn.zst"

      "qcom/sc8280xp/LENOVO/21BX/battmgr.jsn.zst"
      "qcom/qcm6490/battmgr.jsn.zst"

      "qcom/sc8280xp/LENOVO/21BX/audioreach-tplg.bin.zst"

      "qcom/sc8280xp/LENOVO/21BX/adspua.jsn.zst"
      "qcom/qcm6490/adspua.jsn.zst"

      "qcom/sc8280xp/LENOVO/21BX/adspr.jsn.zst"
    ];
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
  systemd.tpm2.enable = false;

  virtualisation.spiceUSBRedirection.enable = true;

  programs.nix-ld.enable = true;

  #virtualisation.libvirtd.enable = true;
  #virtualisation.libvirtd.qemu.ovmf.packages = [ pkgs.OVMFFull.fd ];
  #virtualisation.libvirtd.qemu.swtpm.enable = true;
  ## to run microvms and libvirt together, or in general to be able to use any bridge
  #virtualisation.libvirtd.allowedBridges = [ "all" ];

  #virtualisation.waydroid.enable = true;

  #virtualisation.podman.enable = true;
  #virtualisation.podman.autoPrune.enable = true;

  #services.tlp = {
  #  enable = true;
  #  settings = {
  #    RUNTIME_PM_ON_AC = "on";
  #    RUNTIME_PM_ON_BAT = "auto";
  #    USB_AUTOSUSPEND = 1;
  #  };
  #};
  services.power-profiles-daemon.enable = true;

  services.udev.extraRules = ''
    # wifi mac address
    ACTION=="add", SUBSYSTEM=="net", KERNELS=="0006:01:00.0", RUN+="${pkgs.iproute2}/bin/ip link set dev $name address 00:00:af:1e:ee:ef"
    # libcamera access to dma_heaps unresolved
    ACTION=="add", SUBSYSTEM=="dma_heap", KERNEL=="linux,cma", GROUP="video", MODE="0660"
    ACTION=="add", SUBSYSTEM=="dma_heap", KERNEL=="system", GROUP="video", MODE="0660"
  '';

  services.pipewire.extraConfig.pipewire = {
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
            };
          };
        }
      ];
    };
  };

  systemd.network.enable = true;
  networking.useNetworkd = true;

  networking.hostName = "ribes-uva-crispa";
  networking.nftables.enable = true;
  networking.firewall.allowedTCPPorts = [ 22000 ];

  networking.networkmanager.enable = false;
  networking.networkmanager.settings = {
    connectivity = {
      enabled = false;
    };
  };

  networking.wireless.iwd.enable = true;
  networking.wireless.iwd.settings = {
    Network = {
      EnableIPv6 = true;
      RoutePriorityOffset = 300;
    };
    Settings = {
      AutoConnect = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It’s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/6974bc45-7ba8-4be9-94a5-cfa4a4ab320c";
      fsType = "btrfs";
      options = [ "noatime" "nodiratime" "discard" ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/FE80-11C1";
      fsType = "vfat";
    };

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 16*1024;
    }
  ];

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  hardware.enableAllFirmware = lib.mkForce true;

  hardware.graphics.enable = true;
  hardware.graphics.extraPackages = with pkgs; [
    libva-vdpau-driver
    libvdpau-va-gl
    vpl-gpu-rt
  ];

  home-manager.users."${username}" = { pkgs, config, ... }: {
    home.sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = 1;
    };
  };
}
