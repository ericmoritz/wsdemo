from multiprocessing import cpu_count
from os              import fork, getpid
from random          import seed
from time            import time
from binascii        import hexlify

def _reseed_random():
    try:
        from os import urandom
        s = long( hexlify( urandom(16) ), 16)
    except (NotImplementedError, ImportError):
        s = int(time() * 1000) ^ getpid()
    seed( s )

def prefork( num=None, fork=fork ):
    """ Forks current process num times.
        Returns a list of child pids for master or an empty list for a child.
    """
    if not num:
        num = cpu_count()
    pids = []
    for n in range(num):
        pid = fork()
        if pid:
            pids.append( pid )
        else:
            _reseed_random()
            break
    return pids
