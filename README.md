# dotfiles

Each top-level directory in this repo has configuration for one
program.  I symlink them to $HOME using
[stow](https://www.gnu.org/software/stow/)

```
cd dotfiles
for f in *; do stow -t ~ $f; done
```
