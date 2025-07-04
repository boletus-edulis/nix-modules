{
  description = "nixos modules collection";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hydra-test.url = "github:boletus-edulis/hydra-test";
    hydra-test.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, emacs-overlay, hydra-test, ... }@inputs: {
    nixosModules = {
      enable-unfree = import ./enable-unfree.nix;

      baseline-configuration = import ./base-configuration.nix;
      base-home = import ./home.nix;
      #wayland = import ./wayland.nix;
      desktop-general = import ./desktop-general.nix;

      desktopX11 = import ./desktopX11.nix;

      desktop-werkstatt = import ./desktop-werkstatt.nix;

      base-vm-gui = import ./base-vm-hw.nix;

      ribes-uva-crispa = import ./x13s.nix;

      simple-router = import ./simple-router.nix;

      emacs = import ./emacs.nix;
    };

    testpackages = hydra-test.packages.aarch64-linux;

    nixosConfigurations.dummySystem = nixpkgs.lib.nixosSystem rec {
      system = "aarch64-linux";
      specialArgs = {
        inherit inputs;
        inherit system;
        username = "Us0r";
        cpkgs = hydra-test.packages.aarch64-linux;
      };
      modules = [
        home-manager.nixosModules.home-manager
        self.nixosModules.baseline-configuration
        self.nixosModules.enable-unfree
        self.nixosModules.ribes-uva-crispa
        self.nixosModules.base-home
        self.nixosModules.desktopX11
      ];
    };

    hydraJobs = {
      dummySystem-rebuild = inputs.nixpkgs.lib.customisation.hydraJob
        self.nixosConfigurations.dummySystem.config.system.build.nixos-rebuild;
      dummySystem-toplevel = inputs.nixpkgs.lib.customisation.hydraJob
        self.nixosConfigurations.dummySystem.config.system.build.toplevel;
    };
  };
}
