"""This is an Erlang port that manages starting and stopping the
server processes for us"""
import subprocess
import sys

for message in sys.stdin:
    print message
