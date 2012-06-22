"""This is an Erlang port that manages starting and stopping the
server processes for us"""
#!/usr/bin/python

import sys
import time
import subprocess
import os
import unittest
import signal
import logging
import re

log = logging.getLogger("server_manager")

HERE = os.path.abspath(os.path.dirname(__file__))
PROJECT_ROOT = os.path.join(HERE, "../")


## Public API

# -spec server_status(str) -> subprocess.Popen
def start_server(name):
    """Starts the server process"""
    old_cwd = os.getcwd()
    os.chdir(os.path.join(PROJECT_ROOT, "competition"))
    args = ["bash", "./server-%s.sh" % name]
    pid = subprocess.Popen(args,
                           stdin=subprocess.PIPE,
                           stderr=subprocess.PIPE,
                           preexec_fn=os.setsid)
    os.chdir(old_cwd)
    return pid


# -spec server_status(subprocess.Popen) -> bool
def server_status(server):
    """Returns true if the server process is still running"""
    if server is not None:
        return server.poll() is None
    else:
        return False


# -spec stop_server(subprocess.Popen) -> returncode :: int
def stop_server(server):
    """Stops the server and blocks until the server process exits"""
    os.killpg(server.pid, signal.SIGTERM)
    return server.wait()

def route_command(message_key, command, args, state):
    if command == "start":
        server_name = args
        state.server = start_server(server_name)
        state.server_type = server_name
        send_message(message_key, "started")
    elif command == "stop":
        if not server_status(state.server):
            send_error(message_key, "server not running")
        else:
            stop_server(state.server)
            state.server = None
            send_message(message_key, "server stopped")
    elif command == "connections":
        hostname = args
        try:
            conn=get_connections(hostname)
            send_message(message_key, str(conn))
        except Exception, e:
            log.exception("connections")
            send_error(message_key, "error %s" % (str(e)))
    elif command == "memusage":
        if server_status(state.server):
            try:
                rss=get_rss(state.server.pid)
                send_message(message_key, str(rss))
            except Exception, e:
                log.exception("memusage")
                send_error(message_key, "error %s" % (str(e)))
        else:
            send_error(message_key, "server not running")
    elif command == "pid":
        if server_status(state.server):
            send_message(message_key, "%s" % (state.server.pid))
        else:
            send_error(message_key, "server not running")
    elif command == "status":
        if server_status(state.server):
            send_message(message_key, "running: %s" % (state.server_type, ))
        else:
            send_message(message_key, "stopped")
    else:
        import pdb; pdb.set_trace()
        send_error(message_key, "invalid command")


class State(object):
    server = None
    server_type = None


def main():
    state = State()

    try:
        while True:
            line = sys.stdin.readline()
            if not line: break
            command_bits = split_command(line)

            if command_bits:
                message_key, command, args = command_bits
                route_command(message_key, command, args, state)
            else:
                send_error("", "usage {message_key}:{command}:{args}")

            # And finally, lets flush stdout because we are communicating with
            # Erlang via a pipe which is normally fully buffered.
            sys.stdout.flush()
    finally:
        # stop the server if it still running
        if server_status(state.server):
            stop_server(state.server)

## Internal

def send_message(message_key, msg):
    print "%s:__message__:%s" % (message_key, msg)

def send_error(message_key, error):
    print "%s:__error__:%s" % (message_key, error)

def split_command(line):
    bits = line.strip().split(":", 2)
    if len(bits) < 2:
        return False
    elif len(bits) == 2:
        message_key, command = bits
        return message_key, command, ""
    else:
        message_key, command, args = bits
        return message_key, command, args

# OSX's netstat ips look like 127.0.0.0.8000 (really?)
OSX_IP_PAT = re.compile(r"(\d+\.\d+\.\d+\.\d+)\.(\d+)")
def fix_osx_ips(ip):
    match = OSX_IP_PAT.match(ip)
    if match:
        return "%s:%s" % (match.groups())
    else:
        return ip

def get_connections(hostname):
    lines = subprocess.check_output(["netstat", "-n"]).splitlines()
    rows = (line.split() for line in lines if "ESTABLISHED" in line)
    rows = (row for row in rows if fix_osx_ips(row[4]) == hostname)
    return len(list(rows))
    
        
def get_rss(pid):
    return sum(map(int,subprocess.check_output(["ps", "-o rss=", "-g", str(pid)]).split()))

class TestServerManager(unittest.TestCase):

    def test_start_server(self):
        pid = start_server("erlang-cowboy")
        time.sleep(1)
        self.assertTrue(pid.poll() is None)
        ps_out = subprocess.check_output(["ps", "aux"])
        self.assertTrue("-s wsdemo" in ps_out)

        stop_server(pid)
        ps_out = subprocess.check_output(["ps", "aux"])
        self.assertTrue("-s wsdemo" not in ps_out)

if __name__ == '__main__':
    main()
