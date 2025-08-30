{
  description = "Your new nix config";

  inputs = {
    # nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    claude-code.url = "github:sadjow/claude-code-nix?ref=1bcd0973b255ae8d77a003a79d195fb83461972d";

    # home-manager
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    claude-code,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
  in {

    # To rebuild, run: 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      personal-laptop-vm = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/hosts/personal-laptop-vm
          {
            nixpkgs.overlays = [ claude-code.overlays.default ];
          }
        ];
      };

      personal-workstation = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/hosts/personal-workstation
          {
            nixpkgs.overlays = [ claude-code.overlays.default ];
          }
        ];
      };

      work-laptop-vm = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/hosts/work-laptop-vm
          {
            nixpkgs.overlays = [ claude-code.overlays.default ];
          }
        ];
      };

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
            nixpkgs.overlays = [ claude-code.overlays.default ];
          }
        ];
      };
    };
  };
}
