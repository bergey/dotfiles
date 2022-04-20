# -*- mode: sh; -*-

alias duh='du -shc *'
alias largest='du -shc * | sort -h'

if [ "$(uname)" = "Darwin" ]; then
    alias emacs='open ~/.nix-profile/Applications/Emacs.app --args'
elif [ -d /etc/nixos ]; then
    alias install='nix-env -i'
    alias up='sudo nixos-rebuild switch --upgrade'
else
    alias install='sudo aptitude install'
    alias up='apt update; apt upgrade -y'
    alias apt='sudo aptitude'
fi

# xrandr commands
alias xr2="xrandr --output HDMI-1 --left-of eDP-1 --mode 1920x1080"
alias xr1="xrandr --output HDMI-1 --off"

# emacs
alias edit='emacsclient -nw'
alias save='emacsclient -e "(save-some-buffers t)"'

# networking
alias wscan='nmcli dev wifi list'
alias wup='nmcli conn up'
alias pong='ping google.com'

# maybe a better version: https://github.com/commercialhaskell/stack/issues/1843
alias 'stack-all-dependencies'="stack ls dependencies | sed 's/ /-/' | grep  \"$(pwd)\|rts\" -v | xargs stack unpack"
alias 'stack-build=stack build --test --no-run-tests'

# git
alias shortlog='git log --pretty=format:"%ai %s"'
# report directories (and files, uselessly) in the current dir not registered with mr
alias mrdiff='diff <(ls) <(sed -nE "s/\[(.*)\]/\1/p" .mrconfig | sort)'

# 'system' utils
alias serve='python3 -m http.server'

alias screenshot='import -window root $(date +%F)-screenshot.jpg'
alias lock='xscreensaver-command --lock'
alias trr=transmission-remote

alias 'make-virtualenv'='virtualenv virtualenv --prompt "($(basename $(pwd))) "'

# file / application shortcuts
alias pylab='ipython --pylab'

alias snapshot='sudo zfs snapshot zpool/crypt/home@$(date -u +%FT%TZ)'
alias backup='borg create /mnt/babel/$(hostname)::$(date +%F) ~ --exclude sh:**/.stack-work --exclude sh:**/.stack --one-file-system'

# docker / kubernetes
alias k=kubectl
alias kl='kubectl --context=local'
alias d=docker

alias vm='gcloud compute instances'
alias gssh='gcloud compute ssh --ssh-flag=-A'

. ~/dotfiles/nix/bash_aliases
