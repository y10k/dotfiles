#!/usr/local/bin/bash

# Startup file
export BASH_ENV=$HOME/.bashrc

# Path
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin:$HOME/bin:$HOME/web/photon/bin

# File permission
umask 22

# terminal settings
if [ -t 0 ]; then
  if [ -n "${TMUX}" ]; then
    stty erase '^?' intr '^C' susp '^Z'
  else
    stty erase '^H' intr '^C' susp '^Z'
  fi
fi

# Locale
#export LANG=ja_JP.UTF-8        # default LANG is used.
export LC_TIME=C

# Editor and Pager
case "$EMACS" in
'') # in Terminal
  export EDITOR=vi
  export PAGER=lv
  ;;
*) # in Emacs
  export EDITOR=emacsclient
  export PAGER=cat
  ;;
esac

# LV
export LV="-c -d -Ia -Ku8 -Ou8"

# FTP
export FTP_PASSIVE_MODE=YES

# Perl
export PERL_BADLANG=

# RCS
export RCSINIT=-zLT

# CVS
export CVS_RSH=ssh

# Ports site
export MASTER_SITE_OVERRIDE='ftp://ring.htcn.ne.jp/pub/FreeBSD/distfiles/${DIST_SUBDIR}/'

# Java
# export JDK_HOME=/usr/local/java
# export JAVA_HOME=$JDK_HOME
# export JAVA_CLASS=/usr/local/share/java
# #export JAVA_COMPILER=shujit
# export PATH=$PATH:$JAVA_HOME/bin
# export CLASSPATH=.:$HOME/java/work:$HOME/java/classes:$JAVA_CLASS/classes
# for jar in {$HOME/java,$JAVA_CLASS,$JAVA_HOME}/classes/*.{zip,jar}; do
#     if [ -f $jar ]; then
#       CLASSPATH=$CLASSPATH:$jar
#     fi
# done
export JAVA_HOME=/usr/local/jdk1.6.0_06
export PATH="$JAVA_HOME/bin:$PATH"
export JRUBY_HOME=$HOME/ruby/jruby-1.2.0
export PATH="$PATH:$JRUBY_HOME/bin"

# HOME PAGE
export WWW_HOME='http://www.hatena.ne.jp/'
export WWW_HOME='http://a.hatena.ne.jp/y10k/'

# Rsync
export RSYNC_RSH=ssh

# PostgreSQL
export PATH=$PATH:/usr/local/pgsql/bin
export PGLIB=/usr/local/pgsql/lib
export PGDATA=/usr/local/pgsql/data

# ssh-agent forwarding in GNU Screen session
if [ -n "$PS1" ]; then          # for interactive shell
  saved_ssh_agent_sock="${HOME}/.ssh/agent_sock"
  saved_ssh_x11_forwarding="${HOME}/.ssh/x11_forwarding"
  unix_socket_tunnel="${HOME}/git_work/unix_socket_tunnel/unix_socket_tunnel"

  if [ -n "${SCREEN_SESSION}" ]; then # add to .screenrc: setenv SCREEN_SESSION 1
    export SSH_AUTH_SOCK="${saved_ssh_agent_sock}"

    ssh_x11_forwarding_reload() {
      if [ -f "${saved_ssh_x11_forwarding}" ]; then
        . "${saved_ssh_x11_forwarding}"
      fi
    }
    ssh_x11_forwarding_reload
  else
    if shopt -q login_shell && [ -n "${SSH_AUTH_SOCK}" ] && [ -S "${SSH_AUTH_SOCK}" ]; then
      rm -f "${saved_ssh_agent_sock}"
      "${unix_socket_tunnel}" "${SSH_AUTH_SOCK}" "${saved_ssh_agent_sock}" &
    fi

    if [ -n "${DISPLAY}" ]; then
      echo "export DISPLAY=${DISPLAY}" >"${saved_ssh_x11_forwarding}"
    else
      rm -f "${saved_ssh_x11_forwarding}"
    fi
  fi
fi

# Docker Toolbox for Windows
case "$(uname -r)" in
  *Microsoft*)
    export DOCKER_HOST=tcp://192.168.99.100:2376
    export DOCKER_CERT_PATH=/mnt/c/Users/toki/.docker/machine/certs
    export DOCKER_TLS_VERIFY=1
    export DOCKER_SSL_VERSION=TLSv1_2
    export COMPOSE_TLS_VERSION="${DOCKER_SSL_VERSION}"
    ;;
esac

if [ -n "$PS1" ]; then
  # Aliases
  alias h='history 25'
  alias j='jobs -l'
  alias la='ls -a'
  alias lf='ls -FA'
  alias ll='ls -lA'
  alias diff='diff -u'
  alias cmd='/mnt/c/Windows/System32/cmd.exe'
  alias explorer='/mnt/c/Windows/explorer.exe'
  alias git_apply="${HOME}/dotfiles/git_apply.sh"

  case "$EMACS" in
  '')
    alias ls='ls -F --color=auto'
    ;;
  *)
    alias ls='ls -F'
    ;;
  esac

  # Shell options
  HISTSIZE=1000
  HISTFILESIZE=1000
  MAILPATH=/var/mail/toki

  # Prompt
  PS1='\u@\h(\!)$ '
fi

# Local Variables:
# indent-tabs-mode: nil
# End:
