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

#my configurations
bindkey -e
source ~/.config/shell/zshbindings
setopt INTERACTIVE_COMMENTS


#environment variables
export XDG_CONFIG_HOME="$HOME/.config"
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:$HOME/.scripts:$HOME/.scripts/window:$HOME/.scripts/sys:$HOME/.scripts/selectors:$HOME/.scripts/music"

export EDITOR="emacsclient -t"
#"/usr/bin/emacs -nw"
export VISUAL="emacsclient -create-frame --alternate-editor=''"
#"emacs"
export TERMINAL="urxvt -e tmux"

export GTK_THEME="Adwaita:dark"
export URXVT_PERL_LIB="$HOME/.config/urxvt/perl/"

#for scripts
export JOT_DIR="$HOME/Documents/Notes"
export TD_DIR="$HOME/Documents/Days"
export EMAIL="thunderbird"
export FILEBROWSER="browse"
export NINEFLAGS="-bg #fdf6e3 -fg #222222 -font 'UbuntuMono-R' -popup -teleport" #6A6B6D 181A26
export MUSIC="$HOME/Music"
export VIDEO="$HOME/Documents/Videos"
export PICTURE="$HOME/Documents/Pictures"
export WEB="firefox"


export bar="$HOME/Projects/code/lojbar/lojbar"
alias -g lemon="lemonbar -p -a 50 -f 'ubuntu mono'"

#aliases & functions

#abbreviations:
alias e="emacsclient -t"
alias blather="$HOME/.programs/blather/Blather.py"
alias m="ncmpcpp"
z(){
    zathura "${@}" & disown
}
#alias e="emacs"

#global
alias -g xs='$(xsel -o)'



#files, etc
alias zrc="$EDITOR ~/.zshrc"
alias token="cat $JOT_DIR/ocaml_token.txt | xsel -bi"
alias sepia="cat $JOT_DIR/sepia.txt | xsel -bi"
alias cover="feh --scale-down -g 599x377 --image-bg black --reload=1 .scripts/output/cover.jpg"





#utils
alias reload="source ~/.zshrc"
cl(){
    cd "${@}" && ls --color
}

space(){ #only a function because of quoting nightmares
	df -h | grep sda4 | awk '{print $3 " of " $2 " (" $5 ") used. " $4 " remaining."}'
}



#configurations
alias dash="rlwrap dash"

emacs(){
    #/usr/bin/emacs "${@}"  & disown
    emacsclient -create-frame --alternate-editor="" "${@}" & disown
}
alias ls="ls --color"
alias ocaml="rlwrap ocaml"


#corrections
alias clear="echo 'Try Ctrl+L'"
alias vim="echo 'Try "e"'"
#"emacs -nw"




#memes etc
alias lofi="mpv 'https://www.youtube.com/watch?v=5qap5aO4i9A'"






alias mdl='youtube-dl -i -f bestaudio\[ext=m4a\] --embed-thumbnail -o "~/Music/%(title)s.%(ext)s"'
alias vdl-sub='youtube-dl -i -f worst -o "~/Documents/Videos/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s" --write-auto-sub'
alias vdl='youtube-dl -i -f worst -o "~/Documents/Videos/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s"'



#scrobbling
alias love='mpc sendmessage mpdas love'
alias unlove='mpc sendmessage mpdas unlove'


# git stuff
alias gst='git status'
alias gadd='git add'
alias gcom='git commit'
alias gpush='git push'
alias gdiff='git diff'
alias gls='git ls-files'
alias guntracked='git ls-files --exclude-standard --others'
function gcfg(){
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


flac2mp3here(){
    find . -print0 | xargs -0 -I '{}' ffmpeg -i '{}' '{}'.mp3
}


#
#alias tlmgr='/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode'





#zsh enhancements
#----

#colours!
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#history: autosuggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# opam configuration
test -r /home/l-acs/.opam/opam-init/init.zsh && . /home/l-acs/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
