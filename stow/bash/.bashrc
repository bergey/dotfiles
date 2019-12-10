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

# time, command number, hostname, return code, $ sign
export PS1="\t \# \h \$? \$ "

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
    awk "NR==1 || \$1 ~ /$1/"
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/usr/local/stow/google-cloud-sdk/path.bash.inc' ]; then source '/usr/local/stow/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/usr/local/stow/google-cloud-sdk/completion.bash.inc' ]; then source '/usr/local/stow/google-cloud-sdk/completion.bash.inc'; fi

if [ -f "$HOME/code/simspace/ci/deploy/portal" ]; then
    eval $($HOME/code/simspace/ci/deploy/portal env)
    export POOL_ALLOCATION=2965-2969
fi

# For Arduino & other Java GUIs
# http://www.simonrichter.eu/blog/2016-11-01-arduino-tiling-window-manager.html
export _JAVA_AWT_WM_NONREPARENTING=1

eval "$(direnv hook bash)"
