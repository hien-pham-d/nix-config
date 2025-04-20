# default target must be the first target in this file.
default: hm-switch

hm-switch:
	home-manager switch --flake .#hienphamduc@nixos

os-rebuild:
	sudo nixos-rebuild switch --flake .#nixos

# delete both nixos and home-manager generations
# update the boot loader
hm-tidy:
	nix-collect-garbage --delete-older-than 30d
	sudo nixos-rebuild boot --flake .#nixos

# list nixos and home-manager's generations
list-generations:
	nix-env --list-generations
	home-manager generations
