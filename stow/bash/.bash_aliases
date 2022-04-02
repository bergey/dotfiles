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

# simspace
alias range-server='export RANGE_HOST=$(ifconfig | awk '"'"'$2 ~ /^192.168.(207|206|205)/  {print $2;}'"'"'); stack exec range-server 2>&1 > $(date -u +"%Y-%m-%dT%H:%M:%SZ")-range-server.log'
alias docker-rmq='docker run -d -p 127.0.0.1:5672:5672 -p 15672:15672 --env RABBITMQ_DEFAULT_USER=guest --env RABBITMQ_DEFAULT_PASS=guest rabbitmq:3.8.3-management'

# docker / kubernetes
alias k=kubectl
alias kl='kubectl --context=local'
alias kt='kubectl --context=tkg-test-cluster01-editor'
alias kb='kubectl --context=kubes-beta'
alias ks='kubectl --context=scalability'
alias kq='kubectl --context=qa'
alias kx='kubectl --context=tkg-vxr1-cluster01-viewer'
alias kxe='kubectl --context=vxr-editor'
alias d=docker
alias pg-scalability='ks port-forward svc/postgres 54323:5432'
alias pg-qa='kq port-forward svc/postgres 54321:5432'
alias pg-beta='kb port-forward svc/postgres 54320:5432'

. ~/dotfiles/nix/bash_aliases
