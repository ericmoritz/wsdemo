from multiprocessing import cpu_count
from os              import fork

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
            break
    return pids
