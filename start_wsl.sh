#!/bin/sh -x

sudo service rsyslog start
sudo service ssh start
sudo service screen-cleanup start

# not used Ubuntu 18.04 on Windows Subsystem for Linux
#sudo sudo cgroupfs-mount
#sudo service docker start

# avoid ruby's segmentation fault on emacs. why?
screen
