#!/usr/local/bin/bash
# ---
# $Id$
#

# Startup file
export BASH_ENV=$HOME/.bashrc

# Path
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin:$HOME/bin

# File permission
umask 22

# terminal settings
[ -t 0 ] && stty erase "^H" intr "^C" susp "^Z"

# Locale
export LANG=ja_JP.EUC
export LC_TIME=C

# Editor and Pager
case "$EMACS" in
'') # in Terminal
  export EDITOR=/usr/local/bin/nvi
  export PAGER=lv;;
*) # in Emacs
  export EDITOR=emacsclient
  export PAGER=cat;;
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
case "`hostname`" in
hellboy.*)
  export CVSROOT=:ext:babayaga.plutonian.ne.jp:/home/toki/cvsroot;;
*)
  export CVSROOT=/home/toki/cvsroot;;
esac

# Ports site
export MASTER_SITE_OVERRIDE='ftp://ring.htcn.ne.jp/pub/FreeBSD/distfiles/${DIST_SUBDIR}/'

# Java
export JDK_HOME=/usr/local/java
export JAVA_HOME=$JDK_HOME
export JAVA_CLASS=/usr/local/share/java
export JAVA_COMPILER=shujit
export PATH=$PATH:$JAVA_HOME/bin
export CLASSPATH=.:$HOME/java/work:$HOME/java/classes:$JAVA_CLASS/classes
for jar in {$HOME/java,$JAVA_CLASS,$JAVA_HOME}/classes/*.{zip,jar}; do
    if [ -f $jar ]; then
	CLASSPATH=$CLASSPATH:$jar
    fi
done

# HOME PAGE
export WWW_HOME=http://www.freedom.ne.jp/toki/

# Ruby
export RUBY_HOME=$HOME/ruby
export RUBYLIB=$RUBY_HOME/lib:$RUBY_HOME/lib/i386-freebsd:$RUBY_HOME/site_ruby:$RUBY_HOME/site_ruby/i386-freebsd

# Name servers
export NS_NEWEB=210.132.91.129

# Rsync
export RSYNC_RSH=ssh

# PostgreSQL
export PATH=$PATH:/usr/local/pgsql/bin
export PGLIB=/usr/local/pgsql/lib
export PGDATA=/usr/local/pgsql/data

# Aliases
alias h='history 25'
alias j='jobs -l'
alias la='ls -a'
alias lf='ls -FA'
alias ll='ls -lA'
alias ls='ls -F'
alias diff='diff -u'
alias vi=/usr/local/bin/nvi
alias view=/usr/local/bin/nview

# Shell options
HISTSIZE=1000
HISTFILESIZE=1000
MAILPATH=/var/mail/toki

# Prompt
PS1='\u@\h(\!)$ '
