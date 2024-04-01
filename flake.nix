{
  description = "nixos modules collection";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    hydra-test.url = "github:boletus-edulis/hydra-test";
    hydra-test.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: {
    nixosModules = {
      enable-unfree = import ./enable-unfree.nix;

      baseline-configuration = import ./base-configuration.nix;
      base-home = import ./home.nix;
      wayland = import ./wayland.nix;

      ribes-uva-crispa = import ./x13s.nix;
    };
  };
}
