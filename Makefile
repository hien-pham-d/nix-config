# default target must be the first target in this file.
default: hm-switch

hm-switch:
	home-manager switch --flake .#hienphamduc@nixos

os-rebuild:
	sudo nixos-rebuild switch --flake .#nixos
