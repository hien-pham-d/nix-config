# Agent Guidelines for nix-config

This document provides guidelines for AI coding agents working in this NixOS configuration repository.

## Repository Overview

This is a personal NixOS and Home Manager flake-based configuration managing multiple machines:
- `personal-laptop-vm` (aarch64-linux)
- `personal-workstation` (x86_64-linux)
- `work-laptop-vm` (aarch64-linux)

**Architecture:**
- `flake.nix` - Main flake configuration with inputs and outputs
- `hosts/common/` - Shared configuration across all hosts
- `hosts/{hostname}/` - Host-specific configurations
- `packages/` - Custom package definitions
- `Makefile` - Common build/deploy commands

## Build, Test, and Deploy Commands

### Testing Configuration
```bash
# Validate flake syntax without building (fast check)
make test

# Watch for changes and auto-test
make loop
```

### Deploying Changes
```bash
# Set HOST environment variable (required)
export HOST=personal-laptop-vm  # or personal-workstation, work-laptop-vm

# Deploy both NixOS and home-manager (default)
make rebuild-all

# Deploy only home-manager changes
make hm-switch

# Deploy only NixOS system changes
make os-rebuild
```

### Single Component Testing
```bash
# Test flake evaluation
nix flake check

# Build specific host without activating
nix build .#nixosConfigurations.personal-laptop-vm.config.system.build.toplevel

# Build specific home-manager config without activating
nix build .#homeConfigurations."hienphamduc@personal-laptop-vm".activationPackage

# Check flake structure
nix flake show
```

### Maintenance Commands
```bash
# Update nixpkgs-unstable (for opencode/latest packages)
make update-nixpkgs-unstable

# Clean old generations (30+ days) and update bootloader
make tidy

# List all generations
make list-generations
```

## Code Style Guidelines

### Nix Language Style

#### File Organization
```nix
# File header with description (optional but recommended)
{ config, pkgs, lib, ... }:

{
  # Imports first
  imports = [
    ./module1.nix
    ./module2.nix
  ];

  # Configuration options
  option.enable = true;
  
  # Comments for sections
  # More configuration
}
```

#### Indentation and Formatting
- **Indentation:** 2 spaces (no tabs)
- **Line length:** No strict limit, but keep readable (~100-120 chars preferred)
- **Attribute sets:** Use consistent spacing
  ```nix
  # Good
  { foo = "bar"; baz = "qux"; }
  
  # Good (multi-line)
  {
    foo = "bar";
    baz = "qux";
  }
  ```

#### Comments
- Use `#` for single-line comments
- Place comments above the code they describe
- Use inline comments sparingly, only for clarification
  ```nix
  # Enable networking
  networking.networkmanager.enable = true;
  
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];  # Google DNS servers
  ```

#### String Formatting
- Use double quotes `"` for simple strings
- Use multi-line strings `''` for longer content
  ```nix
  simpleString = "hello";
  
  multiLine = ''
    line 1
    line 2
  '';
  ```

#### Lists and Attribute Sets
- Lists: Use spaces between elements
  ```nix
  packages = [ pkg1 pkg2 pkg3 ];
  
  # Multi-line for many items
  packages = [
    pkg1
    pkg2
    pkg3
  ];
  ```

- Use `with pkgs;` for package lists to reduce repetition
  ```nix
  home.packages = with pkgs; [
    git
    ripgrep
    fd
  ];
  ```

#### Naming Conventions
- **Variables/attributes:** camelCase for local, kebab-case for NixOS options
- **Functions:** camelCase (e.g., `mkNixosHost`, `mkHomeConfig`)
- **Hosts/machines:** kebab-case (e.g., `personal-laptop-vm`)
- **Files:** kebab-case (e.g., `hardware-configuration.nix`)

### Module Structure

#### Host-Specific Configuration
```nix
# hosts/{hostname}/nixos/default.nix or home-manager/default.nix
{ host, lib, ... }: {
  imports = [
    ../../common/nixos/core.nix  # Import common config
    ./hardware-configuration.nix
  ];
  
  # Override common settings with mkForce if needed
  networking.hostName = lib.mkForce "hostname";
  
  # Host-specific options
}
```

#### Using Nix Library Functions
- `lib.mkForce` - Override a value with highest priority
- `lib.mkDefault` - Set a default that can be overridden
- `lib.mkIf` - Conditional configuration
- `lib.mkMerge` - Merge multiple configurations

### Package Management

#### Adding Packages
1. Add to `home.packages` in `hosts/common/home-manager/core.nix` for user packages
2. Add to `environment.systemPackages` in `hosts/common/nixos/core.nix` for system packages
3. Group related packages with comments:
   ```nix
   home.packages = with pkgs; [
     # Development
     git
     ripgrep
     
     # Python
     python313
     python313Packages.pip
   ];
   ```

#### Custom Packages
- Place in `packages/{package-name}/default.nix`
- Add overlay in `flake.nix`:
  ```nix
  customOverlay = final: prev: {
    custom-package = final.callPackage ./packages/custom-package { };
  };
  ```

### Shell Aliases
- Add to `programs.zsh.shellAliases` in home-manager config
- Keep aliases short and memorable
- Group by purpose (git, navigation, etc.)

## Error Handling

### Flake Evaluation Errors
- Run `nix flake check` to catch syntax errors early
- Use `nix eval` to debug specific attributes:
  ```bash
  nix eval .#nixosConfigurations.personal-laptop-vm.config.networking.hostName
  ```

### Build Errors
- Check the full build log
- For package-specific errors, try building the package alone:
  ```bash
  nix build nixpkgs#package-name
  ```

### Deployment Issues
- If `nixos-rebuild` fails, check system logs: `journalctl -xe`
- If `home-manager switch` fails, use `-b backup` flag (already in Makefile)
- Rollback if needed:
  ```bash
  sudo nixos-rebuild switch --rollback
  home-manager switch --rollback
  ```

## Best Practices

1. **Test before committing:** Always run `make test` before committing changes
2. **Keep common configs DRY:** Put shared configuration in `hosts/common/`
3. **Document non-obvious choices:** Add comments for workarounds or special cases
4. **Pin package versions carefully:** Use overlays for specific version requirements
5. **Update inputs regularly:** Run `nix flake update` periodically
6. **Backup before major changes:** The Makefile creates backups automatically
7. **Use conventional commits:** Format: `feat:`, `fix:`, `chore:`, `docs:`
8. **Clean up periodically:** Run `make tidy` to remove old generations

## Common Patterns

### Adding a New Host
1. Create `hosts/{hostname}/nixos/default.nix`
2. Create `hosts/{hostname}/home-manager/default.nix`
3. Add to `flake.nix` nixosConfigurations and homeConfigurations
4. Generate hardware config: `nixos-generate-config`

### Overlays for Package Overrides
```nix
customOverlay = final: prev: {
  package-name = prev.package-name.override {
    enableFeature = true;
  };
};
```

### Version Pinning Pattern
See `work-laptop-vm` config for pinning specific package versions via fetchTarball.

## Environment Variables

- `HOST` - Required for Makefile targets (hostname)
- Set in shell: `export HOST=personal-laptop-vm`

## File Locations

- **Dotfiles:** `hosts/common/home-manager/dotfiles/`
- **Scripts:** `~/.local/scripts/`
- **Common NixOS config:** `hosts/common/nixos/core.nix`
- **Common Home Manager config:** `hosts/common/home-manager/core.nix`
