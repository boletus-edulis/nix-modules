{ config, pkgs, system, nixpkgs, ... }:
let
  overlay-unstable = final: prev: {
    unstable = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
  };
in
{
  nixpkgs.overlays = [ overlay-unstable ];
  nixpkgs.config.allowUnfree = true;
}
