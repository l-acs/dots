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
alias delete-whitespace="tr -d '[:blank:]'"
alias squeeze-whitespace="tr '[:blank:]' '\t' | tr -s '\t'"
alias remove-empty="sed '/^[ 	]*$/d'"
alias shrink-tabs="tr -s '[:blank:]'"
alias strip-newlines="tr -d '\n'"
alias number='cat -n'

function row () {
  cat /dev/stdin | sed -n "$1"p
}

alias col='cut -f'

alias first='head -n '
alias last='tail -n '

function drop-first() { n="$1"; shift 1; tail -n "+$((n + 1))" $* }
function drop-last()  { n="$1"; shift 1; head -n "-$n" $* }

alias choose='rofi -dmenu'
alias apply='xargs'
alias stdin-ntfy='apply -0 notify-send'

# for use in conditionals
function defined? () {
  out="$(which "$@")"
  result="$?"
  echo "$out" | sed "/$@ not found/d"
  return $result
}


function date-suffix () {
   case "$(($1 % 100))" in
      11) echo "$1th";;
      12) echo "$1th";;
      13) echo "$1th";;
      *)  case "$(($1 % 10))" in
             1) echo "$1st";;
             2) echo "$1nd";;
             3) echo "$1rd";;
             *) echo "$1th";;
          esac ;;
   esac
}

alias today-with-suffix='echo "$(date +%A,\ %B) $(date-suffix $(date +%-d)), $(date +%Y)"'


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
    if [ -n "$(xsel -o | delete-whitespace)" ]; then xsel; else xsel -b; fi
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

alias ipacopy="
    number ~/projects/domainspeak/node_modules/ipa-parser/src/data/{vowels,consonants,alternatives}.json   |
    shrink-tabs      |
    choose -i        |
    col 2- -d'\"'    |
    col 2 -d'['      |
    col 1 -d ']'     |
    col 1 -d'{'      |
    tr -d '[\",:\n]' |
    sed 's/null//'   |
    clip"

alias ipacopycycle="ipacopy -a; ipacopy -a; ipacopy -a"

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
	  history | last 1 | col 3- -d' ' | clip
	  ;;
       1)
	  history | last "$1" | col 3- -d' ' | clip
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
    alias $host="TERM=rxvt; ssh $host"
done

alias -g ndclab="~/projects/external/ndclab/git"


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
    df -h | grep '/$' | awk '{print $3 " of " $2 " (" $5 ") used. " $4 " remaining."}'
}

function math() {
  emacsclient -e "( $* )"
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

function current-song()
{
    mpc current -f '%file%' | sed "s|^|$MUSIC/|"
}

function current-song-dir()
{
    current-song | xargs -0 dirname
}

# go to current song folder
function nav-to-current-song()
{
    current-song-dir | clip ; cd xb
}

function open-if-exists() { [ -f "$1" ] && eval ${VISUAL} '"$1"' }


function get-current-lyrics()
{
    dir="$(current-song-dir)"
    stem="$(basename "$(current-song)" | sed "s/\.\(mp3\|flac\|m4a\)$//")"

    (ls "$dir/$stem.lrc"               ||
	 ls "$MUSIC/.lyrics/$stem.lrc" ||
	 ls "$dir/$stem.txt"           ||
	 ls "$MUSIC/$(mpc current -f '%artist% - %title%').txt" ) 2>/dev/null

}

function open-lyrics-if-exist()
{
    open-if-exists "$(get-current-lyrics)"
}

alias mpc-position='mpc status | grep / | col 2 -d/ | squeeze-whitespace | col 2'

function current-couplet()
{
    after_count=1
    [ -n "$1" ] && after_count="$1"

    now="0$(mpc-position)"
    sort "$(get-current-lyrics)" <(echo "[$now]") |
	grep "\[$now\]" -B1 -"A$after_count"      |
	grep -v "\[$now\]"                        |
	col 2- -d\]
}

function ntfy-current-couplet()
{
    current-couplet $1 | stdin-ntfy --hint=string:x-dunst-stack-tag:couplet
}

# cover art
function save-current-art-to-file()
{
    ffmpeg -i "$(current-song)" "$(current-song-dir)"/cover.jpg
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

function emojisel()
{
  choose -i $(emojicopy)
}

function emojisel-interactive()
{
  emoji ' ' | choose -i | col 1 | delete-whitespace | strip-newlines | clip
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

alias cp="cp -i"
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

# trilium exports
alias trilium-unzip='[ -f root.zip ] && (rm -r root \!\!\!meta.json && unzip root.zip && rm root.zip && gadd .); gst'


### docker ###
alias dlogs='docker container logs'

function matching-containers ()
{
    docker container ls --all |
	sed '/^CONTAINER/d' |
	col 1 -d' ' |
	while read id;
	    do
		[ -n "$(docker container inspect "$id" | grep -i "$*")" ] &&
			echo Container \'"$id"\' matches "$*"\; \
			    its image might be \
			    "$(docker container inspect "$id" | grep image -i | col 4 -d'"' | last 1)";
            done
}


function non-matching-containers ()
{
    docker container ls --all |
	sed '/^CONTAINER/d' |
	col 1 -d' ' |
	while read id;
	    do
		[ -z "$(docker container inspect "$id" | grep -i "$*")" ] &&
			echo Container \'"$id"\' does not match "$*"\; \
			    its image might be \
		  	    "$(docker container inspect "$id" | grep image -i | col 4 -d'"' | last 1)";
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

alias mpcsel='mpc playlist | number | shrink-tabs | choose -i | col 1 | apply mpc play'

function kitaab-vocab ()
{
    grep 'alkitaabtextbook.com/[-._/a-z%A-Z0-9]*/[a-zA-Z0-9\-_]*.mp3' ~/ara/alkitaabtextbook.com/part2/3e/lesson$1/index.html -o | sed "s|^|$HOME/ara/|" | apply mpv
}

function kitaab-vocab-mpvc ()
{
    grep 'alkitaabtextbook.com/[-._/a-z%A-Z0-9]*/[a-zA-Z0-9\-_]*.mp3' ~/ara/alkitaabtextbook.com/part2/3e/lesson$1/index.html -o | sed "s|^|$HOME/ara/|" | apply mpvc add
}


alias kitaab="z $HOME/Documents/fall-2021/fiu/al-kitaab-two.pdf"


alias condacontainer='docker ps | grep anaconda | col 1 -d\ '
alias condastop='docker container stop $(condacontainer)'
alias condaurl='docker logs $(condacontainer) | grep http | col 7- -d" "  | last 1'
alias condashell='docker exec -it $(condacontainer) /bin/bash'


alias clock='tty-clock -b -t -c'
alias pomo='~/projects/python/pomo/add-date.sh >/dev/null ; pomo'
alias pomodoro:='pomo $POMO_DEFAULT_DURATION'

function avg-daily-pomos-in-2022 ()
{
    math / \
        $(drop-first 687 ~/.scripts/output/pomo.log | grep -c ^Pomodoro).0 \
        $(date +%j).0
}

function avg-weekly-pomos-in-2022 ()
{
    math / \
        $(drop-first 687 ~/.scripts/output/pomo.log | grep -c ^Pomodoro).0 \
        $(date +%W).0
}



## ex. # of pomos this year per day of the work week
# math / $(avg-weekly-pomos-in-2022) 5
