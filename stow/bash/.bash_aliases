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
alias forcepush='git push --force-with-lease'

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

alias listeners='lsof -i -P | headgrep LISTEN'

# docker / kubernetes
alias k=kubectl
alias kl='kubectl --context=local'
alias d=docker

# Braze environments
alias olaf='kubectl --context=k8s.cluster-001.d-use-1.braze.com-opsengineer'
alias sweeney='kubectl --context=k8s.cluster-001.s-use-1.braze.com-opsengineer'
alias us1='kubectl --context=k8s.cluster-001.p-use-1.braze.com-opsengineer'
alias us2='kubectl --context=k8s.cluster-002.p-use-1.braze.com-opsengineer'
alias us3='kubectl --context=k8s.cluster-003.p-use-1.braze.com-opsengineer'
alias us4='kubectl --context=k8s.cluster-004.p-use-1.braze.com-opsengineer'
alias us5='kubectl --context=k8s.cluster-005.p-use-1.braze.com-opsengineer'
alias us6='kubectl --context=k8s.cluster-006.p-use-1.braze.com-opsengineer'
alias us7='kubectl --context=k8s.cluster-007.p-use-1.braze.com-opsengineer'
alias eu1='kubectl --context=k8s.cluster-001.p-euc-1.braze.eu-opsengineer'
alias eu2='kubectl --context=k8s.cluster-002.p-euc-1.braze.eu-opsengineer'
alias us_redirect='kubectl --context k8s.region-001.p-use-1.braze.com-opsengineer -n url-shortener-redirect-service'
alias us_register='kubectl --context k8s.region-001.p-use-1.braze.com-opsengineer -n url-shortener-registration-service'
alias eu_redirect='kubectl --context k8s.region-001.p-euc-1.braze.eu-opsengineer -n url-shortener-redirect-service'
alias eu_register='kubectl --context k8s.region-001.p-euc-1.braze.eu-opsengineer -n url-shortener-registration-service'
alias olaf_register='kubectl --context k8s.region-001.d-use-1.braze.com-opsadmin -n url-shortener-registration-service'
alias olaf_redirect='kubectl --context k8s.region-001.d-use-1.braze.com-opsadmin -n url-shortener-redirect-service'
alias sweeney_register='kubectl --context k8s.region-001.s-use-1.braze.com-opsengineer -n url-shortener-registration-service'
alias sweeney_redirect='kubectl --context k8s.region-001.s-use-1.braze.com-opsengineer -n url-shortener-redirect-service'
alias usw_register='kubectl --context k8s.test-001.d-usw-2.braze.com-opsengineer -n url-shortener-registration-service'
alias usw_redirect='kubectl --context k8s.test-001.d-usw-2.braze.com-opsengineer -n url-shortener-redirect-service'

alias first_pod="awk 'NR > 1 && !/nginx/ { print $1; }'"
alias deploy_image='jq .spec.template.spec.containers[0].image'
alias pod_image='jq .spec.containers[0].image'
alias deploy_container='jq .spec.template.spec.containers[0]'

alias vm='gcloud compute instances'
alias gssh='gcloud compute ssh --ssh-flag=-A'

alias json_lines='jq -r ".[]"'

. ~/dotfiles/nix/bash_aliases
