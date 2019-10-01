emacs:
	nix-env -if emacs.nix

update:
	cp ${HOME}/code/active/nix/nixpkgs-snapshot.json .
	git reset # make sure we aren't commiting anything else
	git add nixpkgs-snapshot.json
	git commit -m 'update nixpkgs snapshot'

