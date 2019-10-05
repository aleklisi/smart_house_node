# Install Erlang on RPi

sudo apt-get update && sudo apt-get install erlang erlang-dev -y

# Install Erlang Serial

cd ~/Documents/Erlang/

git clone https://github.com/tonyg/erlang-serial

cd erlang-serial/

make

sudo DESTDIR=/usr/lib make install

# Install rebar3

wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

./rebar3 local install

export PATH=/home/pi/.cache/rebar3/bin:$PATH

# Setup smart_house_node

Copy project

cd Documents/

mkdir Erlang

cd Erlang/

git clone https://github.com/aleklisi/smart_house_node.git

cd smart_house_node/

rebar3 compile

Go to priv/sys.comfig

Change exometer params to your setup.
For development use:

rebar3 shell

For permanent setup make relase...

# Conncet arduino

Use this program as reference example

https://create.arduino.cc/editor/aleklisi/46e24f74-904d-4589-aea4-ae4e932535b6/preview

