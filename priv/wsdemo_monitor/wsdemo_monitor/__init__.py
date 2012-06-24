import re
import subprocess
from supervisor import rpcinterface
from supervisor.xmlrpc import Faults
from supervisor.xmlrpc import RPCError


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
    return sum(map(int,subprocess.check_output(["ps", "-o", "rss=", "-g", str(pid), "-p", str(pid)]).split()))


class WSDemoMonitor(rpcinterface.SupervisorNamespaceRPCInterface):
    def __init__(self, supervisord, **config):
        rpcinterface.SupervisorNamespaceRPCInterface.__init__(self, supervisord)

    def memusage(self, name):
        info = self.getProcessInfo(name)

        if info['pid'] == 0:
            raise RPCError(Faults.NOT_RUNNING)

        return get_rss(info['pid'])

    def connections(self, hostname):
        return get_connections(hostname)


def make_rpcinterface(supervisord, **config):
    return WSDemoMonitor(supervisord, **config)

