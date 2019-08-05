#!/usr/local/bin/bash

# Startup file
export BASH_ENV=$HOME/.bashrc

# Path
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin:$HOME/bin:$HOME/web/photon/bin

# File permission
umask 22

# terminal settings
if [ -t 0 ]; then
  stty erase "^H" intr "^C" susp "^Z"
fi

# Locale
export LANG=ja_JP.UTF-8
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
export LV="-d -Ia -Kej -Oej"

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

# Ruby
export RUBYOPT=rubygems

# Java
# export JDK_HOME=/usr/local/java
# export JAVA_HOME=$JDK_HOME
# export JAVA_CLASS=/usr/local/share/java
# #export JAVA_COMPILER=shujit
# export PATH=$PATH:$JAVA_HOME/bin
# export CLASSPATH=.:$HOME/java/work:$HOME/java/classes:$JAVA_CLASS/classes
# for jar in {$HOME/java,$JAVA_CLASS,$JAVA_HOME}/classes/*.{zip,jar}; do
#     if [ -f $jar ]; then
# 	CLASSPATH=$CLASSPATH:$jar
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

if [ -n "$PS1" ]; then
  # Aliases
  alias h='history 25'
  alias j='jobs -l'
  alias la='ls -a'
  alias lf='ls -FA'
  alias ll='ls -lA'
  alias diff='diff -u'

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
