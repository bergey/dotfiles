stow:
  mkdir -p ~/.local
  mkdir -p ~/.config
  for d in $(ls stow); do stow -t ~ -d stow $d; done
  ln -s $(pwd)/emacs ~/.emacs.d

unstow:
  for package in $(ls stow); do stow --target ~ --dir stow --delete $package; done
  rm ~/.emacs.d
