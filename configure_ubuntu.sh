sudo apt-get install python-software-properties
sudo apt-add-repository ppa:chris-lea/node.js
sudo apt-get update
sudo apt-get install emacs python-dev python-setuptools build-essential erlang libevent-dev git
sudo easy_install ws4py gevent
sudo cp etc/sysctl.conf /etc/
sudo sysctl -p
mkdir src
cd src
curl http://nodejs.org/dist/v0.6.18/node-v0.6.18.tar.gz
cd node-v0.6.18
./configure && make && sudo make install
cd ..
npm install websockets

