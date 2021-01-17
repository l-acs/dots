# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: %s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[.-_]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' menu select=1
zstyle ':completion:*' prompt '%e error(s) found. Select a replacement:'
zstyle ':completion:*' select-prompt %SScrolling active: %p%s
zstyle ':completion:*' substitute 1
zstyle :compinstall filename '/home/l-acs/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall


# Lines configured by zsh-newuser-install
HISTFILE=~/.config/shell/histfile
HISTSIZE=1000000
SAVEHIST=10000000000
setopt appendhistory autocd
unsetopt beep nomatch notify
# End of lines configured by zsh-newuser-install


### zsh enhancements ###
bindkey -e
source ~/.config/shell/zshbindings
setopt INTERACTIVE_COMMENTS

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # colours!
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh # history: autosuggestions

# environment variables
source ~/.config/shell/profile


### abbreviations: ###
alias e="emacsclient -t"
alias blather="$HOME/.programs/blather/Blather.py"
alias m="ncmpcpp"
alias nf=neofetch

function x()
{
    xdg-open "${@}" &
}

function z()
{
    zathura "${@}" & disown
}

function _myxsel()
{
    if [ -n "$(xsel -o | tr -d '[:blank:]')" ]; then xsel; else xsel -b; fi
}

alias -g xs='"$(_myxsel)"'
alias -g xb='"$(xsel -b)"'

alias lemon="lemonbar -p -a 50 -f 'ubuntu mono' -f 'Font Awesome 5 Free' -f 'Font Awesome 5 Brands' -f 'Font Awesome 5 Free Solid'"


### files, etc ###
alias zrc="$EDITOR ~/.zshrc"
alias token="cat $JOT_DIR/ocaml_token.txt | xsel -bi"
alias sepia="cat $JOT_DIR/sepia.txt | xsel -bi"
alias cover="feh --auto-zoom --keep-zoom-vp .scripts/output/cover.png"
playlists="$HOME/.config/mpd/playlists"


### utils ###
alias reload="source ~/.zshrc"
alias anon='unset HISTFILE'
alias mountnosudo='sudo mount -o umask=000'

function cl ()
{
    cd "${@}" && ls --color
}

function space() #only a function because of quoting nightmares
{ 
	df -h | grep sda4 | awk '{print $3 " of " $2 " (" $5 ") used. " $4 " remaining."}'
}

alias battery='cat /sys/class/power_supply/BAT0/capacity'
alias sysbright='brightnessctl set'
 
alias screencast='ffmpeg -f x11grab -video_size 1920x1080 -framerate 25 -i :0 -f alsa -i default -c:v libx264 -preset ultrafast -c:a aac '

function flac2mp3here()
{
    find . -print0 | xargs -0 -I '{}' ffmpeg -i '{}' '{}'.mp3
}

function playsliststoipod()
{
    # puts them in the right place, makes the path absolute, removes invalid characters, and 'converts' to m3u8
    for playlist in "$playlists"/*.m3u; do
	newname="$(basename "$playlist" | sed 's/[:\\]//g')"8
	sed 's|^|/music/|' "$playlist" > /run/media/l-acs/LSAHAR\'S\ IP/Playlists/"$newname"
    done
}

# scrobbling
alias love='mpc sendmessage mpdas love'
alias unlove='mpc sendmessage mpdas unlove'

# youtube
alias lofi="mpv 'https://www.youtube.com/watch?v=5qap5aO4i9A' & disown"

alias mdl='youtube-dl -i -f bestaudio\[ext=m4a\] --embed-thumbnail -o "$MUSIC/%(title)s.%(ext)s"'
alias vdl-sub='youtube-dl -i -f worst -o "$VIDEO/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s" --write-auto-sub'
alias vdl='youtube-dl -i -f worst -o "$VIDEO/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s"'


### configurations ###
alias dash="rlwrap dash"
alias ocaml="rlwrap ocaml"
test -r /home/l-acs/.opam/opam-init/init.zsh && . /home/l-acs/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

function emacs()
{
    emacsclient -create-frame --alternate-editor="" "${@}" & disown
}

alias ls="ls --color"
alias pgrep="pgrep -f -a"


### git ###
alias gst='git status'
alias gadd='git add'
alias gcom='git commit'
alias gpush='git push'
alias gdiff='git diff'
alias gls='git ls-files'
alias guntracked='git ls-files --exclude-standard --others'

function gcfg()
{
    case $# in
	0)
	    name='l-acs'
	    email='lucas.sahar@mail.mcgill.ca'
	    ;;
	1)
	    echo Usage: \`gcfg\` or \`gcfg NAME EMAIL\`
	    return 1
	    ;;
	2)
	    name="$1"
	    email="$2"
	    ;;
    esac
    git config user.name "$name"
    git config user.email "$email"

}


### miscellaneous ###

# jot logs
eval $(grep suffix= ~/.scripts/jot)
for i in trash grocery; do
    touch "$JOT_DIR/$i.$suffix"
    alias "$i"="eval jot "$i" \$(date) - "
done
for i in 'tickler/new-music?' guitar; do
    alias "$i"="jot "$i" \"\$(mpc current)\""
done
