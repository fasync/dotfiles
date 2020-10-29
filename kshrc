# Rust Env
. $HOME/.cargo/env
export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/src

# ENV
export AUTO_SYNC_PATH="$HOME/.org"
export GIT_REP_PATH="/home/shell/Documents/Git:/home/shell/Prog"
export cxx=clang++
export PATH="/usr/lib/llvm/8/bin/:$PATH"
export cc=clang
export EDITOR=vim

# Color output
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias startx='startx -- vt1'

# Useful stuff
alias wttr='curl -sS  http://wttr.in/Uelzen | less'

# alias
alias youdl='youtube-dl --extract-audio --audio-format vorbis --audio-quality 0 --yes-playlist --add-metadata --metadata-from-title "%(artist)s - %(title)s" -o "~/Music/%(title)s.%(ext)s"'
alias youdl_video='youtube-dl -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4"'

# Settings
set -o emacs
set -o braceexpand
set -o trackall
(set -o globstar) 2>/dev/null &&
    set -o globstar
(set -o histexpand) 2>/dev/null &&
    set -o histexpand
HISTFILE=$HOME/.ksh_history
