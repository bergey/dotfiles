.PHONY: stow
stow:
	for d in $$(ls stow); do stow -t ~ -d stow $$d; done
