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
    # ssh agent
    echo $SSH_AGENT_PID > $HOME/.ssh/agent.pid
    if [ -L $HOME/.ssh/auth_sock ]; then
	rm -f $HOME/.ssh/auth_sock
    fi
    ln -s $SSH_AUTH_SOCK $HOME/.ssh/auth_sock
    ssh_agent_remove() {
	rm -f $HOME/.ssh/agent.pid
	rm -f $HOME/.ssh/auth_sock
    }
    trap ssh_agent_remove EXIT

    # load startup file
    if [ -f $HOME/.bash_profile ]; then
	. $HOME/.bash_profile
    fi

    # set root window
    display -window root $HOME/imgs/ys_01.jpg

    # start Window Manager
    fvwm2 &
    wm_pid=$!

    # start some nice programs
    skkinput &
    xautolock &
    tclock -geometry -1+1 &
    xeyes -geometry 120x80-0+50 &
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
