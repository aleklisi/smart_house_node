#!/bin/bash

# This script allows to quickly update code.
# Requires to add the computer to trusted hosts of updated device.
# Example use ./update.sh 192.160.0.123 node_name
IP=$1
NODE_NAME=$2
COOKIE="house"
HOST="pi@$IP"

echo $HOST

EXPORTS="export IP=$1; export NODE_NAME=$2; export COOKIE=house"
echo $EXPORTS

ssh -t $HOST $EXPORTS

ssh -T $HOST << EOF
    echo $IP
    echo $NODE_NAME
    echo $COOKIE
    cd Documents/Erlang/smart_house_node
    pkill screen
    echo "Node killed"
    mkdir /home/pi/tmp
    cp -R ./priv /home/pi/tmp
    echo "Config files saved"
    git clean -ffdx
    git fetch
    git reset --hard origin/master
    git pull
    echo "Code updated to latest"
    cp -R /home/pi/tmp/priv ./
    echo "Config files reverted"
    rm -r /home/pi/tmp
    echo "Tmp config dir removed"
EOF

ssh -T $HOST << EOF2
    cd Documents/Erlang/smart_house_node
    ./start.sh
    echo "Node started"
EOF2
