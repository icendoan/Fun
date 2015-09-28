eval $(dircolors ~/.dircolours)

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

