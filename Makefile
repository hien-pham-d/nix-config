# default target must be the first target in this file.
# default: rebuild-all
.PHONY: test

test:
	nix flake check --no-build

loop:
	fd --color never --regex '.nix$$|Makefile' . | entr -c bash -c 'make test > debug.log 2>&1'

rebuild-all: os-rebuild hm-switch

hm-switch:
	home-manager switch -b backup --flake .#hienphamduc@nixos

os-rebuild:
	sudo nixos-rebuild switch --flake .#$${HOST:?}

# delete both inactive nixos and home-manager generations
# update the boot loader
tidy:
	nix-collect-garbage --delete-older-than 30d
	sudo nixos-rebuild boot --flake .#$${HOST:?}

# list nixos and home-manager's generations
list-generations:
	nix-env --list-generations
	home-manager generations
