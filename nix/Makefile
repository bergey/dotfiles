OLD := 7d

.PHONY: all os os-update user global

user: global

all: user os

update:
	@./update.sh
	make user

os:
	sudo nixos-rebuild switch

os-update:
	sudo nixos-rebuild switch --upgrade

global:
	nix-env --no-build-output -if global.nix

prune:
	date > prune.log
	sudo nix-collect-garbage --delete-older-than ${OLD} 2>&1 | awk '{ print $$0 >> "prune.log"; dots+=1 }; dots < 80 { printf "." }; dots >= 80 { printf ".\n"; dots=0 }'

roots:
	for r in $$(nix-store --gc --print-roots | awk '$$1 ~ /^\/home\/bergey/ {print $$1;}'); do \
    du -shc $$(nix-store -qR $$r 2>/dev/null) | awk -v r="$$r" '$$2 ~ /total/ {print $$1, r;}' ;  done \
    | sort -h

iso:
	nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix
