#!/bin/bash

# install PyPy
PYPY_VERSION="1.9"
PYPY_PREFIX="/usr/local"
PYPY_URL="http://cdn.bitbucket.org/pypy/pypy/downloads/pypy-$PYPY_VERSION-linux.tar.bz2"

if [ ! -d "$PYPY_PREFIX/pypy-$PYPY_VERSION" ]; then
    echo ">>> Installing PyPy-$PYPY_VERSION into $PYPY_PREFIX/pypy-$PYPY_VERSION <<<"
    pushd "$PYPY_PREFIX"
        # bootstrap
        wget -O - "$PYPY_URL" | sudo tar xj
        PYPY="pypy-$PYPY_VERSION/bin/pypy"
        wget -O - "http://python-distribute.org/distribute_setup.py" | sudo "$PYPY"
        sudo "pypy-$PYPY_VERSION/bin/easy_install" pip
        # symlinks
        sudo mkdir -p bin
        sudo ln -s "../$PYPY" bin/pypy
        sudo ln -s "../pypy-$PYPY_VERSION/bin/pip" bin/pip-pypy
        # packages
        sudo pip-pypy install tornado
    popd
fi
echo "=== PyPy-$PYPY_VERSION is installed at $PYPY_PREFIX/pypy-$PYPY_VERSION ==="
