#!/bin/bash

# This script allows to start a project with a single command when configured
# There is an assumption project is setup in /home/pi/Documents/Erlang/smart_house_node/

export PATH=/home/pi/.cache/rebar3/bin:$PATH
export PATH=/bin/:/usr/bin:$PATH

export IP_ADDRESS=$((ifconfig wlan0 || ifconfig eth0) \
    | grep -oP 'inet ([\d|\.]*)' \
    | grep -oP '([\d|\.]*)')

echo "IP is $IP_ADDRESS"

export NODE_NAME=$(cat /home/pi/Documents/Erlang/smart_house_node/priv/sys.config \
    | grep -oP 'prefix, "([a-zA-Z1-9]*)"' \
    | grep -oP '"([a-zA-Z1-9]*)"' \
    | grep -oP '([a-zA-Z1-9]*)')

echo "NODE_NAME is $NODE_NAME"

export COOKIE="house"

cd /home/pi/Documents/Erlang/smart_house_node/

rebar3 compile

screen -d -m rebar3 shell --name "$NODE_NAME@$IP_ADDRESS" --setcookie "$COOKIE"
