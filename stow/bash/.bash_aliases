# -*- mode: sh; -*-

# some more ls aliases
alias ll='ls -alFh'
alias la='ls -A'
alias d='ls'

alias duh='du -shc *'
alias largest='du -shc * | sort -h'
alias dudot='ls -a|grep -v '\''\.$'\''|xargs du -sh'

if [ "$(uname)" = "Darwin" ]; then
    alias install='sudo port install'
    alias up='sudo port upgrade installed'
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

alias mail="mbsync -a; ~/code/utility/sort_mail.py'"
# sentweek broken as of 2019-09-18
# alias sentweek='notmuch search tag:sent $(date -v-mon -v-1w +%s)..'

# mount helpers
alias phone='cadaver 192.168.1.8:8080'

# haskell aliases
alias cabalw="cabal install --ghc-options='-Wall -Werror'"
alias cabalc="cabal configure"
alias cabalb="cabal build"
alias cabalf="cabal build --ghc-options='-fno-code -fforce-recomp'"
alias cabaldeps='cabal install --only-dependencies'
alias ghcsandbox='ghc -global-package-db -no-user-package-db -package-db .cabal-sandbox/$(uname -m)-linux-ghc-$(ghc --numeric-version)-packages.conf.d/ -O2'
# alias ghc78sandbox='/usr/local/stow/ghc-7.8.1-rc2/bin/ghc -global-package-db -no-user-package-db -package-db .cabal-sandbox/x86_64-linux-ghc-7.8.0.20140228-packages.conf.d/ -O2'
alias unregister='cabal sandbox hc-pkg unregister'
alias sand-shared='cabal sandbox init --sandbox=../.cabal-sandbox'
alias dryrun='cabal install --dry-run -v3'
alias cabalbin='cabal install --bindir=$HOME/.cabal/bin --data-dir=$HOME/.cabal/share'
alias cabal-update='cabal  --no-require-sandbox update'
alias trynix='/home/bergey/code/active/ghcjs/reflex-platform/work-on ./ghcjs.nix ./.'

# maybe a better version: https://github.com/commercialhaskell/stack/issues/1843
alias 'stack-all-dependencies'="stack ls dependencies | sed 's/ /-/' | grep  \"$(pwd)\|rts\" -v | xargs stack unpack"
alias 'stack-cabal-freeze'="echo -n 'constraints: ' >> cabal.config && stack list-dependencies | sed -e 's/ / == /' -e 's/$/,/' -e 's/^/             /' >> cabal.config"
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
alias dvd='mplayer dvd://1 -dvd-device /dev/dvd'

alias 'make-virtualenv'='virtualenv virtualenv --prompt "($(basename $(pwd))) "'

# file / application shortcuts
alias pylab='ipython --pylab'
alias off='libreoffice'
alias ashrae='zathura ~/annex/non-fiction/ASHRAE/2009\ ASHRAE\ Handbook/FRONT/NAV.pdf &'
alias hvac='zathura ~/annex/non-fiction/ASHRAE/2012\ ASHRAE\ Handbook/FRONT/S12_NAV.pdf &'

alias snapshot='sudo zfs snapshot zpool/crypt/home@$(date -u +%FT%TZ)'
alias backup='borg create /mnt/babel/$(hostname)::$(date +%F) ~ --exclude sh:**/.stack-work --exclude sh:**/.stack --one-file-system'

alias urldecode='python -c "import sys;import urllib.parse as ul;print(ul.unquote_plus(sys.argv[1]))"'

# simspace
alias range-server='export RANGE_HOST=$(ifconfig | awk '"'"'$2 ~ /^192.168.(207|206|205)/  {print $2;}'"'"'); stack exec range-server 2>&1 > $(date -u +"%Y-%m-%dT%H:%M:%SZ")-range-server.log'
alias docker-rmq='docker run -d -p 127.0.0.1:5672:5672 -p 15672:15672 --env RABBITMQ_DEFAULT_USER=guest --env RABBITMQ_DEFAULT_PASS=guest rabbitmq:3.8.3-management'
alias ts-gen='TSC=tsc PRETTIER=prettier stack run ts-gen'

# docker / kubernetes
alias k=kubectl
alias kl='kubectl --context=local'
alias kt='kubectl --context=tkg-test-cluster01-editor'
alias kb='kubectl --context=kubes-beta'
alias ks='kubectl --context=ev2-editor --namespace=scalability'
alias kq='kubectl --context=qa'
alias kx='kubectl --context=tkg-vxr1-cluster01-viewer'
alias kxe='kubectl --context=tkg-vxr1-cluster01-editor'
# alias ks='kubectl --kubeconfig ~/.kube/scaling-portal'
alias d=docker

. ~/dotfiles/nix/bash_aliases
