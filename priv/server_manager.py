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

def send_message(msg):
    print "__message__:%s" % (msg)

def send_error(error):
    print "__error__:%s" % (error)

def main():
    server = None
    server_type = None

    while True:
        line = sys.stdin.readline()
        if not line: break
        message = line.strip()

        if message.startswith("start "):
            server_name = message.lstrip("start ")
            server = start_server(server_name)
            server_type = server_name
            send_message("started")
        elif message == "stop":
            if not server_status(server):
                send_error("server not running")
            else:
                stop_server(server)
                server = None
                send_message("server stopped")
        elif message.startswith("connections "):
            hostname = message.lstrip("connections ")
            try:
                conn=get_connections(hostname)
                send_message(str(conn))
            except Exception, e:
                log.exception("connections")
                send_error("error %s" % (str(e)))
        elif message == "memusage":
            if ensure_started(server):
                try:
                    rss=get_rss(server.pid)
                    send_message(str(rss))
                except Exception, e:
                    log.exception("memusage")
                    send_error("error %s" % (str(e)))
                
        elif message == "pid":
            if ensure_started(server):
                send_message("%s" % (server.pid))
        elif message == "status":
            if server_status(server):
                send_message("running: %s" % (server_type, ))
            else:
                send_message("stopped")
        else:
            send_error("invalid command")

        # And finally, lets flush stdout because we are communicating with
        # Erlang via a pipe which is normally fully buffered.
        sys.stdout.flush()


## Internal
def get_connections(hostname):
    lines = subprocess.check_output(["netstat", "-n"]).splitlines()
    rows = (line.split() for line in lines if "ESTABLISHED" in line)
    rows = (row for row in rows if row[4] == hostname)
    return len(list(rows))
    
def ensure_started(server):
    if server_status(server):
        return True
    else:
        send_error("server not running")
        return False
    
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
