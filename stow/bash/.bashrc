# -*- mode: sh; -*-

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ -n "${INSIDE_EMACS}" ] && [ $TERM != "eterm-color" ]; then
    # shell started by M-x shell, but not by M-x term
    export MANPAGER=cat
    export TERM=vt100           # color codes are supported
    export PAGER=cat
else
    export MANPAGER=less
fi

# from https://github.com/NixOS/nix/issues/1268
function restore_prompt_after_nix_shell() {
    if [ -n "$IN_NIX_SHELL" ]; then
        nix_env=$(echo $out | sed -E 's,/nix/store/[^-]*-(bootstrap-)?,,')
        if echo $out | grep -q bootstrap- ;
        then nix_env="\[\e[35m\][$(echo $nix_env | sed 's,bootstrap-,,')]"
        else nix_env="\[\e[34m\][$nix_env]"
        fi
        PS1="${nix_env} $PROMPT"
        PROMPT_COMMAND=""
    fi
}

PROMPT_COMMAND=restore_prompt_after_nix_shell
# time, command number, hostname, return code, $ sign
PROMPT='\[\e[36m\]\t \# \h $? \[\e[1;37m\]$ \[\e[m\]'
export PS1=$PROMPT

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# enable programmable completion features
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if [ -d ~/.nix-profile/etc/bash_completion.d ]; then
   for f in ~/.nix-profile/etc/bash_completion.d/*; do
       . $f
   done
fi

if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
[ -f /opt/local/share/bash-completion/completions/pass ] && . /opt/local/share/bash-completion/completions/pass
[ -f /usr/share/bash-completion/completions/pass ] && . /usr/share/bash-completion/completions/pass

[ -f /usr/share/autojump/autojump.sh ] && . /usr/share/autojump/autojump.sh
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh


GPG_TTY=$(tty)
export GPG_TTY

. ~/.bash_passwords

# for pass password manager; otherwise it calls xclip with the Clipboard
export PASSWORD_STORE_X_SELECTION=clipboard

[ -f ~/.bash_env ] && . ~/.bash_env

# functions break .xsessionrc

function s3 {
s3cmd put $2 s3://bergey/$1/$2 -P;
}

# photo helpers
function photorefile {
         exiftool '-filename<CreateDate' -d %FT%H:%M:%S%%-c.%%le -r $1
         exiftool '-Directory<CreateDate' -d ~/Library/photos/%Y/%Y-%m -r $1
}

function wepub {
wget $1; ebook-convert $(basename $1) $(basename $1 | sed 's/\.[a-zA-Z]\+$/.epub/'); rm $(basename $1)
}

function diagramwatch {
         while true
               do inotifywait $1.hs && ghcsandbox $1 && $1
               done
}

function povwatch {
         while true
               do inotifywait -e modify $1.hs && cabal exec runhaskell $1 > $1.pov && povray -W1280 -H960 $1.pov
               done
}

function boundtest {
    cabal sandbox delete
    cabal sandbox init
    cabal install --constraint="$1"
}

function headgrep {
    awk "NR==1 || \$0 ~ /$1/"
}

# redirect STDIN to the file passed as $1; print progress dots to STDOUT
function quiet {
    # unescaped: awk '{ print $0 >> "quiet.log"; dots+=1 }; dots < 80 { printf "." }; dots >= 80 { printf ".\n"; dots=0 }'
    awk "{ print \$0 >> \"$1\"; dots+=1 }; dots < 80 { printf \".\" }; dots >= 80 { printf \".\n\"; dots=0 }"
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/usr/local/stow/google-cloud-sdk/path.bash.inc' ]; then source '/usr/local/stow/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/usr/local/stow/google-cloud-sdk/completion.bash.inc' ]; then source '/usr/local/stow/google-cloud-sdk/completion.bash.inc'; fi

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

if [ -f "$HOME/code/simspace/ci/deploy/portal" ]; then
    export PATH="$PATH:$HOME/code/simspace/ci/scripts:$HOME/code/simspace/ci/deploy"
    export POOL_ALLOCATION=2965-2969,2700-2709
fi

# For Arduino & other Java GUIs
# http://www.simonrichter.eu/blog/2016-11-01-arduino-tiling-window-manager.html
export _JAVA_AWT_WM_NONREPARENTING=1

eval "$(direnv hook bash)"

function loc-lang {
    # report lines of code in each sub-directory, in a particular language
    lang="$1"
    {     echo "LOC Directory"
          for d in $(find . -type d -maxdepth 1); do
              echo -n '*' >&2
          count=$(loc $d | grep -i "$lang")
          if [ $? == 0 ]; then echo "$d $count" | awk '{print $7, $1;}'; fi
          done
          echo '' >&2
    } | sort -h | column -t
}

function jq-grep {
    while IFS= read -r line; do
        if [[ ${line:0:1} == "{" ]] ;
        then echo "$line" | jq -c "$1" | sed -e '/^$/d' -e '/^null$/d'
        else echo "$line" | grep "$2"
        fi
    done
}

function datelog {
    stack exec $1 2>&1 > $(date -u +"%Y-%m-%dT%H:%M:%SZ-$1.log")
}

# commit of currently-running pod for a given deployment name
function pod-commit {
    imageID=$(k get deploy $1 -o json | jq -r '.spec.template.spec.containers[0].image' | sed 's,registry.hub.docker.com/,,')
    docker pull $imageID
    commit=$(docker image inspect $imageID | jq -r '.[].ContainerConfig.Labels.git_commit')
    echo $commit
    git show $commit
}

if [ "$(uname)" = "Darwin" ]; then
    function nix-open {
        # Apple Spotlight finds *every* version Nix has installed,
        # but provides no indication which is symlinked from .nix-profile
        application=$1
        open "$HOME/.nix-profile/Applications/${application}.app"
    }
fi

if type kustomize
then source <(kustomize completion bash)
fi

function rename-metrics {
    mv ../metrics.csv.zst .
    zstd -d metrics.csv.zst
    rm metrics.csv.zst
    mv metrics.csv $1
}
