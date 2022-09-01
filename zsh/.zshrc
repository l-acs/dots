# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: %s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[.-_]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' max-errors 2
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

# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # colours!
# source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh # history: autosuggestions

# ubuntu version:
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # colours!
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh # history: autosuggestions

# make emacs shell work
[ $TERM == "dumb" ] && unsetopt zle


# environment variables
source ~/.config/shell/profile

### parsing enhancements ###
alias squeeze-whitespace="tr '[:blank:]' '\t' | tr -s '\t'"

alias strip-newlines="tr -d '\n'"

function row () {
  cat /dev/stdin | sed -n "$1"p
}

alias col='cut -f'

alias first='head -n '
alias last='tail -n '


### abbreviations: ###
alias blather="$HOME/.programs/blather/Blather.py"
alias clj="clojure"
alias e="emacsclient -t"
alias k9="kill -9"
alias m="ncmpcpp"
alias n="nmtui"
alias nord="nordvpn"
alias nc="nordvpn connect"
alias nd="nordvpn disconnect"
alias nst="nordvpn status"
alias nf=neofetch
alias p=ping
alias py=python3

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

function vtmp () {
    () { vim $1 -c startinsert
         cat $1 } =() }

alias -g xs='"$(_myxsel)"'
alias -g xb='"$(xsel -b)"'
alias -g clip='xsel -b'

function cwrite ()
{
    vim $1 -c startinsert
    clip < $1
}

function unicopy () {
    unicode.py $* | clip
}

function copy () {
    echo -n $* | clip
}

# edit a temporary file and write its contents to stdout, copying them to the clipboard
alias cw='cwrite =() && clip'

# copy last line(s) of history to clipboard
function copylast()
{
    case $# in
       0)
	  history | tail -n 1 | row 3- -d' ' | clip
	  ;;
       1)
	  history | tail -n "$1" | row 3- -d' ' | clip
	  ;;
       *)
          return 1
	  ;;
    esac
}

alias lemon="lemonbar -p -a 50 -f 'ubuntu mono' -f 'Font Awesome 5 Free' -f 'Font Awesome 5 Brands' -f 'Font Awesome 5 Free Solid'"


### files, etc ###
alias zrc="vim ~/.zshrc && reload"
alias cover="feh --auto-zoom --keep-zoom-vp .scripts/output/cover.png"
playlists="$HOME/.config/mpd/playlists"

# aliases for each school folder
for folder in "$HOME/s/"*; do
	if [ -d "$folder" ]; then
		base="$(basename "$folder")"
		alias -g "$base"="$folder"
	fi
done

# alias for each nicknamed SSH server
grep ^Host ~/.ssh/config | while read host; do
    host=$(echo $host | sed 's/Host //')
    alias $host="ssh $host"
done


### utils ###
alias reload="source ~/.zshrc"
alias anon='unset HISTFILE'
alias leave='bg %1 ; disown ; exit'
alias mountnosudo='sudo mount -o umask=000'

function cl()
{
    cd "${@}" && ls --color
}

function space() #only a function because of quoting nightmares
{ 
	df -h | /bin/grep '/$' | awk '{print $3 " of " $2 " (" $5 ") used. " $4 " remaining."}'
}

alias battery='cat /sys/class/power_supply/BAT0/capacity'
alias sysbright='brightnessctl set'

# rename a window for e.g. rofi -window
alias wrename='xprop -format _NET_WM_NAME 8u -set _NET_WM_NAME'

### media ###
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

function videotoaudio()
{
    # todo: take more arguments
    ffmpeg -i "$*" -vn -acodec copy "$(echo "$*" | sed 's/\.[a-zA-Z0-9]*$/.aac/')"
}


# go to current song folder
function nav-to-current-song()
{
    mpc current -f '%file%' | sed "s|^|$MUSIC/|" | xargs -0 dirname | clip ; cd xb
}

function open-if-exists() { [ -f "$1" ] && eval ${VISUAL} '"$1"' }

function open-lyrics-if-exist()
{
    current="$(mpc current -f '%file%' | sed "s|^|$MUSIC/|")"
    dir="$(dirname "$current")"
    stem="$(basename "$current" | sed "s/\.\(mp3\|flac\|m4a\)$//")"

    open-if-exists "$dir/$stem.lrc" ||
	   open-if-exists "$dir/$stem.txt" ||
	   open-if-exists "$MUSIC/.lyrics/$stem.lrc" ||
	   open-if-exists "$MUSIC/$(mpc current -f '%artist% - %title%').txt" ||
	   return 1
}

# cover art
function save-current-art-to-file()
{
    ffmpeg -i "$MUSIC/$( mpc current -f '%file%' )" "$MUSIC/$(dirname "$(mpc current -f '%file%')")/cover.jpg"
}

# scrobbling
alias love='mpc sendmessage mpdas love'
alias unlove='mpc sendmessage mpdas unlove'

# youtube
alias lofi="mpv 'https://www.youtube.com/watch?v=5qap5aO4i9A' & disown"

alias mdl='youtube-dl -i -f bestaudio\[ext=m4a\] --embed-thumbnail -o "$MUSIC/%(title)s.%(ext)s"'
alias vdl-sub='youtube-dl -i -f worst -o "$VIDEO/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s" --write-auto-sub'
alias vdl='youtube-dl -i -f worst -o "$VIDEO/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s"'
alias vdl-sub='yt-dlp -o "$VIDEO/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s" --write-auto-sub'
alias vdl='yt-dlp -o "$VIDEO/yt/%(uploader)s/%(upload_date)s %(title)s.%(ext)s"'

# 🤪
function emoji()
{
  yeetgif emoji -l $* 2>&1 | col 2- -d\]
}

function emojicopy()
{
  emoji $* | squeeze-whitespace | row 2 | col 2 | strip-newlines | clip
}

### configurations ###
alias dash="rlwrap dash"
alias ocaml="rlwrap ocaml"
test -r /home/l-acs/.opam/opam-init/init.zsh && . /home/l-acs/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

function emacs()
{
    emacsclient -create-frame --alternate-editor="" "${@}" & disown
}

grepflags="--color --no-messages"
alias fgrep="$(which -p grep) $grepflags" # force a path search so not overridden by function

function grep()
{
    # if there's no standard input, then tell grep to run recursively on its arguments
    if [ -p /dev/stdin ]; then
        fgrep $* /dev/stdin
    else
        fgrep --dereference-recursive $*
    fi
}

ping=$(which -p ping) # force a path search so not overridden by function
function ping()
{

    case $# in
	0)
	    $ping gnu.org
	    ;;
	*)
	    $ping $*
	    ;;
    esac

}

alias crontab="crontab -i"
alias diff="diff --unified --color"
alias du="du -sh"
alias less='less -N'
alias ls="ls --color"
alias pgrep="pgrep -f -a -i"
alias rm="rm -I"
alias tmux="tmux -f ~/.config/tmux/tmux.conf"

### git ###
alias gst='git status'
alias gadd='git add'
alias gcom='git commit'
alias gpull='git pull'
alias gpush='git push'
alias gdiff='git diff'
alias gls='git ls-files'
alias guntracked='git ls-files --exclude-standard --directory --others'

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


function magit()
{
   if [ $# -eq 0 ]; then
      if [ -d .git ]; then
         emacs -eval "(magit)"
      else
         echo 'Not a git repository! Please navigate to one or pass an argument.'
	 return 1
      fi
   else
      emacs -eval "(magit \"$*\")"
   fi
}


### docker ###
alias dlogs='docker container logs'

function matching-containers ()
{
    docker container ls --all |
	sed '/^CONTAINER/d' |
	cut -f1 -d' ' |
	while read id;
	    do
		[ -n "$(docker container inspect "$id" | grep -i "$*")" ] &&
			echo Container \'"$id"\' matches "$*"\; \
			    its image might be \
			    "$(docker container inspect "$id" | grep image -i | cut -f4 -d'"' | tail -n 1)";
            done
}


function non-matching-containers ()
{
    docker container ls --all |
	sed '/^CONTAINER/d' |
	cut -f1 -d' ' |
	while read id;
	    do
		[ -z "$(docker container inspect "$id" | grep -i "$*")" ] &&
			echo Container \'"$id"\' does not match "$*"\; \
			    its image might be \
		  	    "$(docker container inspect "$id" | grep image -i | cut -f4 -d'"' | tail -n 1)";
	    done
}



### music ##

function m3u8tom3u()
{
	for playlist in "$1"/*.m3u8
	do
		newname="$(basename "$playlist" \.m3u8 | sed 's/[:\]//g').m3u"
		echo $playlist
		cat "$playlist"
		echo $newname

		cp "$playlist" "$2/$newname"
	done
}

function movedeezerplaylists()
{
	 ls -1 "$HOME/Music/deemix Music/"*.m3u8  | while read line; do mv "$line" "$HOME/Music/Playlists/$(basename "$line" | sed 's/m3u8$/m3u/')"; done
}

alias mpcsel='mpc playlist | cat -n | rofi -dmenu -i | col 1 | xargs mpc play'

function kitaab-vocab ()
{
    grep 'alkitaabtextbook.com/[-._/a-z%A-Z0-9]*/[a-zA-Z0-9\-_]*.mp3' ~/ara/alkitaabtextbook.com/part2/3e/lesson$1/index.html -o | sed "s|^|$HOME/ara/|" | xargs mpv
}

function kitaab-vocab-mpvc ()
{
    grep 'alkitaabtextbook.com/[-._/a-z%A-Z0-9]*/[a-zA-Z0-9\-_]*.mp3' ~/ara/alkitaabtextbook.com/part2/3e/lesson$1/index.html -o | sed "s|^|$HOME/ara/|" | xargs mpvc add
}


alias kitaab="z $HOME/Documents/fall-2021/fiu/al-kitaab-two.pdf"
CONTAINER_NOTEBOOK_DIR='/opt/notebooks'
alias jupyter='docker run -d -t -p 8888:8888 -v "$HOME/s/webapp/projects/notebooks":"$CONTAINER_NOTEBOOK_DIR" continuumio/anaconda3 /bin/bash -c "mkdir -p "$CONTAINER_NOTEBOOK_DIR" && /opt/conda/bin/jupyter notebook --notebook-dir="$CONTAINER_NOTEBOOK_DIR" --ip=\* --port=8888 --no-browser --allow-root"'

alias condacontainer='docker ps | grep anaconda | cut -f1 -d\ '
alias condastop='docker container stop $(condacontainer)'
alias condaurl='docker logs $(condacontainer) | grep http | cut -f7- -d" "  | tail -n 1'
alias condashell='docker exec -it $(condacontainer) /bin/bash'


alias clock='tty-clock -b -t -c'
alias pomo='~/projects/pomo/add-date.sh >/dev/null ; pomo'
alias pomodoro:='pomo $POMO_DEFAULT_DURATION'
# alias pomodoro='pomodoro:'
