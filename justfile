default: mr update global update-emacs emacs

global:
  #!/usr/bin/env bash
  set -euxo pipefail
  nix-env --no-build-output -if nix/global.nix -A $(hostname)
  # symlink MacOS Applications
  if [[ -d ~/.nix-profile/Applications ]]; then
    for path in ~/.nix-profile/Applications/*; do
    app=$(basename "$path")
      if [[ ! -e /Applications/"$app" ]]
        then ln -s "$path" /Applications/"$app"
      fi
    done
  fi

show-trace:
  nix-env --no-build-output -if nix/global.nix -A $(hostname) --show-trace

update:
  @nix/update.sh
  # TODO only checkout newer commit of nixpkgs git repo & commit here if global rule built successfully

bootstrap:
  nix-build nix/bootstrap.nix
  rm result*

prandtl: update global update-emacs emacs os-update

os:
  sudo nixos-rebuild switch

os-update:
  sudo nixos-rebuild switch --upgrade

OLD := "7d"
dots := quote('
     NL == 1 { lines=0; };
     { print $0 >> "prune.log"; dots+=1 };
     dots < 100 { printf "." };
     dots >= 100 { printf ".\n%s ", lines; dots=0; lines += 100 }
     ')

prune:
  date > prune.log
  sudo nix-collect-garbage --delete-older-than {{OLD}} 2>&1 | awk {{dots}}
  tail -n1 prune.log

roots:
  for r in $(nix-store --gc --print-roots | awk '$1 ~ /^{{escaped_home}}/ {print $1;}'); do \
    du -shc $(nix-store -qR $r 2>/dev/null) | awk -v r="$r" '$2 ~ /total/ {print $1, r;}' ;  done \
    | sort -h

iso:
  nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix

stow:
  mkdir -p ~/.local
  mkdir -p ~/.config
  for d in $(ls stow); do stow -t ~ -d stow $d; done
  ln -s $(pwd)/emacs ~/.emacs.d

unstow:
  for package in $(ls stow); do stow --target ~ --dir stow --delete $package; done
  rm ~/.emacs.d

emacs:
	nix-env -if emacs/emacs.nix

update-emacs:
	cp nix/nixpkgs-snapshot.json emacs
	git reset # make sure we aren't commiting anything else
	git add emacs/nixpkgs-snapshot.json
	if ! git diff --cached --exit-code  --quiet; \
		then git commit -m "$(basename $(pwd)): update nixpkgs snapshot"; \
		else echo "nothing to commit"; \
		fi

mr:
    mr -d {{home}} update

home := env_var('HOME')
escaped_home := replace(home, '/', '\/')
