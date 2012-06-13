sudo apt-get install -y emacs python-dev python-setuptools build-essential erlang-nox libevent-dev git golang mercurial default-jdk
sudo easy_install ws4py gevent

git clone git://github.com/ericmoritz/wsdemo.git wsdemo

sudo cp wsdemo/etc/sysctl.conf /etc/
sudo sysctl -p

mkdir src
pushd src
  curl http://nodejs.org/dist/v0.6.19/node-v0.6.19.tar.gz | tar xz
  pushd node-v0.6.19
    ./configure && make && sudo make install
  popd
popd

npm install websocket
sudo go get code.google.com/p/go.net/websocket

