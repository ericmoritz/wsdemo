from multiprocessing import cpu_count
from os              import fork, getpid, kill, wait
from signal          import signal, SIGINT, SIGTERM, SIGQUIT
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

def prefork( num=None, fork=fork, do_seed=True, do_wait=True, do_kill=True ):
    """ Forks current process num times.

        fork    -- a forking function
        do_seed -- True to reseed a pseudo-random generator of a child
        do_wait -- True to wait for all children to exit
        do_kill -- True to pass a recieved signal (INT,TERM,QUIT) to all children

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
            if do_seed:
                _reseed_random()
            return []

    if do_kill:
        def _pass( sig, frame ):
            for pid in pids:
                #print 'kill( %s, %s )' % (pid, sig)
                kill( pid, sig )
        signal( SIGINT,  _pass )
        signal( SIGTERM, _pass )
        signal( SIGQUIT, _pass )

    _pids = pids[:]
    while do_wait and _pids:
        try:
            pid, sig = wait()
        except OSError:
            continue
        #print '%s has died' % pid
        if pid in _pids:
            _pids.remove( pid )

    return pids
