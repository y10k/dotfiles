#!/bin/sh -x

sudo service rsyslog start
sudo service ssh start
sudo service screen-cleanup start
nohup sudo dockerd >dockerd.log 2>&1 &
