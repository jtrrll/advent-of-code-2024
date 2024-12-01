{
  description = "jtrrll's solutions to Advent of Code 2024.";

  inputs = {
    devenv = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:cachix/devenv";
    };
    env-help = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:jtrrll/env-help";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    snekcheck = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:jtrrll/snekcheck";
    };
  };

  outputs = {flake-parts, ...} @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [./modules];
    };
}
