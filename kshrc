. /etc/ksh.kshrc

# ENV
export cxx=clang++
export cc=clang
export EDITOR='emacsclient -t -a=\"\"'
export PS1='\u::\W $ '

# Useful stuff
alias wttr='curl -sS  http://wttr.in/Uelzen | less'

print() {
    
}
scan() {
    ssh florian@rpi "scanimage -d hpaio:/net/DeskJet_3630_series?ip=192.168.0.112 --resolution 400 --format=png" >> $1
}

# alias
alias youdl='youtube-dl --extract-audio --audio-format vorbis --audio-quality 0 --yes-playlist --add-metadata --metadata-from-title "%(artist)s - %(title)s" -o "~/Music/%(title)s.%(ext)s"'
alias youdl_video='youtube-dl -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4"'

# Settings
set -o braceexpand
set -o trackall
(set -o globstar) 2>/dev/null &&
    set -o globstar
(set -o histexpand) 2>/dev/null &&
    set -o histexpand
HISTFILE=$HOME/.ksh_history
