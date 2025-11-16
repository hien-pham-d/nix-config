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

    claudeCodeAcpOverlay = final: prev: {
      claude-code-acp = final.callPackage ./packages/claude-code-acp { };
    };

    commonOverlays = [
      claude-code.overlays.default
      opencodeOverlay
      claudeCodeAcpOverlay
    ];

    mkNixosHost = hostname: nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs outputs;};
      modules = [
        ./hosts/${hostname}/nixos
        { nixpkgs.overlays = commonOverlays; }
      ];
    };

    mkHomeConfig = {username, hostname, system, extraOverlays ? []}:
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        extraSpecialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/${hostname}/home-manager
          {
            nixpkgs.overlays = commonOverlays ++ extraOverlays;
          }
        ];
      };
  in {

    nixosConfigurations = {
      personal-laptop-vm = mkNixosHost "personal-laptop-vm";
      personal-workstation = mkNixosHost "personal-workstation";
      work-laptop-vm = mkNixosHost "work-laptop-vm";
    };

    home-manager.backupFileExtension = "backup";
    homeConfigurations = {
      "hienphamduc@nixos" = mkHomeConfig {
        username = "hienphamduc";
        hostname = "nixos";
        system = "aarch64-linux";
      };

      "hienphamduc@personal-laptop-vm" = mkHomeConfig {
        username = "hienphamduc";
        hostname = "personal-laptop-vm";
        system = "aarch64-linux";
      };

      "hienphamduc@personal-workstation" = mkHomeConfig {
        username = "hienphamduc";
        hostname = "personal-workstation";
        system = "x86_64-linux";
      };

      "hienphamduc@work-laptop-vm" = mkHomeConfig {
        username = "hienphamduc";
        hostname = "work-laptop-vm";
        system = "aarch64-linux";
        extraOverlays = [
          (final: prev: let
            nodejs_20_10_pkgs = import (builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/2392daed231374d0d1c857cd7a75edd89e084513.tar.gz";
              sha256 = "0qfqia0mcbaxa7zy9djnk5dzhs69pi0k6plgmf1innj8j38kkp0k";
            }) { system = final.system; };
            prisma_nixpkgs = import (builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/59133ee770406f605d61698bc4f1a89efcf461d5.tar.gz";
              # calculate the sha256 by exec: `nix-prefetch-url --unpack ${url:?}`
              sha256 = "0v4bjwfr2zpxdk9s8ncphchr3x52q3v5yvv9wdsp2bnqnh8mpyh1";
            }) { system = final.system; };
          in {
            nodejs = nodejs_20_10_pkgs.elmPackages.nodejs;
            yarn = nodejs_20_10_pkgs.yarn;
            prisma-engines = prisma_nixpkgs.prisma-engines;
          })
        ];
      };
    };
  };
}
