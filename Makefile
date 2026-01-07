# default target must be the first target in this file.
.PHONY: test

default: rebuild-all

test:
	nix flake check --no-build

loop:
	fd --color never --regex '.nix$$|Makefile' . | entr -c bash -c 'make test > debug.log 2>&1'

rebuild-all: os-rebuild hm-switch

hm-switch:
	home-manager switch -b backup --flake .#hienphamduc@$${HOST:?}

os-rebuild:
	sudo nixos-rebuild switch --flake .#$${HOST:?}

# fix opencode outdate issues
update-nixpkgs-unstable:
	nix flake update nixpkgs-unstable

# delete both inactive nixos and home-manager generations
# update the boot loader
tidy:
	nix-collect-garbage --delete-older-than 30d
	sudo nixos-rebuild boot --flake .#$${HOST:?}

# list nixos and home-manager's generations
list-generations:
	nix-env --list-generations
	home-manager generations
