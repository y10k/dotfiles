#!/bin/sh
# $Id$

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11R6/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11R6/lib/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f $userresources ]; then
    xrdb -merge $userresources
fi

if [ -f $usermodmap ]; then
    xmodmap $usermodmap
fi


ssh-agent bash <<'EOF'
    # load startup file
    if [ -f $HOME/.bash_profile ]; then
	. $HOME/.bash_profile
    fi

    # set root window
    case "`hostname`" in
    cernobog.*)
      root_image=$HOME/imgs/ys_01.jpg;;
    hellboy.*)
      root_image=$HOME/imgs/witch2_l.jpg;;
    esac
    [ -n "$root_image" -a -f "$root_image" ] && display -window root "$root_image"

    # start Window Manager
    fvwm2 &
    wm_pid=$!

    # start some nice programs
    skkinput &
    xautolock &
    tclock -geometry -1+1 &
    xeyes -geometry 120x80-0+50 &
    xload -geometry 180x140+0+180 &
    xosview -geometry 200x250+0+333 &
    xcalendar -geometry -0-100 &
    if [ -x /usr/games/fortune ]; then
	/usr/games/fortune | col -b | expand | \
	xmessage -geometry +15-18 -name "Today's message" -file - &
    fi

    # ask ssh pass phrase
    ssh-add

    # start login terminal
    kterm -geometry 100x35+50+30 -T login@`hostname` &

    # wait for Window Manager
    wait $wm_pid
    stat=$?

    # exit
    exit $stat
EOF
