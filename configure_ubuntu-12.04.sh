#!/bin/bash
# Note: pushd/popd only work in bash

sudo apt-get install -y \
    curl emacs python-dev python-setuptools build-essential erlang-nox \
    libevent-dev git golang mercurial openjdk-7-jdk ruby rubygems haskell-platform

sudo easy_install pip ws4py gevent gevent-websocket tornado twisted txws supervisor

# Clone master
git clone git://github.com/ericmoritz/wsdemo.git wsdemo

# Update sysctl
sudo cp wsdemo/etc/sysctl.conf /etc/
sudo sysctl -p

# install the wsdemo_monitor package
sudo easy_install wsdemo/priv/wsdemo_monitor

# install pypy
pushd wsdemo/competition
    if [ ! -d ./pypy-1.9 ]; then
       # TODO make this platform independant
       curl http://cdn.bitbucket.org/pypy/pypy/downloads/pypy-1.9-linux64.tar.bz2 | tar xj
    fi
    curl http://python-distribute.org/distribute_setup.py | ./pypy-1.9/bin/pypy
    ./pypy-1.9/bin/easy_install pip
    ./pypy-1.9/bin/pip install tornado ws4py twisted txws
popd

# install Node
mkdir src
pushd src
  curl http://nodejs.org/dist/v0.8.0/node-v0.8.0.tar.gz | tar xz
  pushd node-v0.8.0
    ./configure && make && sudo make install
  popd
popd

npm install websocket ws
sudo go get code.google.com/p/go.net/websocket
sudo gem install em-websocket

sudo cabal update
sudo cabal install snap-server snap-core websockets websockets-snap

sudo apt-get install -y libadns1-dev
echo -e "y\ny\no conf prerequisites_policy follow\no conf commit" | sudo cpan
sudo cpan Protocol::WebSocket
sudo cpan YAML
sudo cpan EV
sudo cpan EV::ADNS
yes | sudo cpan IO::Stream
