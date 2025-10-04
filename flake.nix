{
  description = "Your new nix config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    claude-code.url = "github:sadjow/claude-code-nix?ref=1bcd0973b255ae8d77a003a79d195fb83461972d";

  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    claude-code,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;

    opencodeOverlay = final: prev: {
      opencode = nixpkgs-unstable.legacyPackages.${final.system}.opencode;
    };

    commonOverlays = [
      claude-code.overlays.default
      opencodeOverlay
    ];

    mkNixosHost = hostname: nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs outputs;};
      modules = [
        ./nixos/hosts/${hostname}
        { nixpkgs.overlays = commonOverlays; }
      ];
    };
  in {

    nixosConfigurations = {
      personal-laptop-vm = mkNixosHost "personal-laptop-vm";
      personal-workstation = mkNixosHost "personal-workstation";
      work-laptop-vm = mkNixosHost "work-laptop-vm";
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#your-username@your-hostname'
    home-manager.backupFileExtension = "backup";
    homeConfigurations = {
      "hienphamduc@nixos" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."aarch64-linux";
        extraSpecialArgs = {inherit inputs outputs;};
        modules = [
          ./home-manager/home.nix
          {
            nixpkgs.overlays = [
              claude-code.overlays.default
              opencodeOverlay
              (final: prev: let
                nodejs_20_10_pkgs = import (builtins.fetchTarball {
                  url = "https://github.com/NixOS/nixpkgs/archive/2392daed231374d0d1c857cd7a75edd89e084513.tar.gz";
                  sha256 = "0qfqia0mcbaxa7zy9djnk5dzhs69pi0k6plgmf1innj8j38kkp0k";
                }) { system = final.system; };
              in {
                nodejs = nodejs_20_10_pkgs.elmPackages.nodejs;
                yarn = nodejs_20_10_pkgs.yarn;
                prisma-engines = nodejs_20_10_pkgs.prisma-engines;
              })
            ];
          }
        ];
      };
    };
  };
}
