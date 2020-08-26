#!/bin/sh -x

sudo killall dockerd
sudo service screen-cleanup stop
sudo service ssh stop
sudo service rsyslog stop
