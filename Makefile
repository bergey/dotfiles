.PHONY: stow
stow:
	for d in $$(ls stow); do stow -t ~ -d stow $$d; done
	ln -s $(shell pwd)/emacs ~/.emacs.d

unstow:
	for package in $$(ls stow); do stow --target ~ --dir stow --delete $$package; done
	rm ~/.emacs.d
