# Rust Env
. $HOME/.cargo/env
export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH

# ENV
export cxx=clang++
export cc=clang
export EDITOR=vi
export PAGER=less
export PS1='\u::\W $ '

# Color output
alias ls='ls --color -F'
alias grep='grep --color'
alias fgrep='fgrep --color'
alias egrep='egrep --color'
alias j='jobs -l'
alias la='ls --color -aF'
alias lf='ls --color -FA'
alias ll='ls --color -lAF'

# Useful stuff
alias wttr='curl -sS  https://wttr.in/Uelzen | less'
scan() {
    ssh florian@rpi "scanimage -d hpaio:/net/DeskJet_3630_series?ip=192.168.0.112 --resolution 300 --format=png" > $1
}

# alias
alias youdl='youtube-dl --extract-audio --audio-format vorbis --audio-quality 0 --yes-playlist --add-metadata --metadata-from-title "%(artist)s - %(title)s" -o "~/Music/%(title)s.%(ext)s"'
alias youdl_video='youtube-dl -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4"'
alias mutt='neomutt'

# Settings
set -o emacs
set -o braceexpand
set -o trackall
(set -o globstar) 2>/dev/null &&
    set -o globstar
(set -o histexpand) 2>/dev/null &&
    set -o histexpand
HISTFILE=$HOME/.ksh_history
