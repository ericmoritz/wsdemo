import sys
import re

PAT = re.compile(r"^server-(.+)\.sh$")
BAD_LINE = re.compile(r"^#!?")
template = file("./server.tmpl.ini").read()

def get_command(fh):
    for line in fh:
        stripped = line.strip()
        if stripped:
            if not BAD_LINE.match(stripped):
                return stripped

for shellscript in sys.argv[1:]:
    server_name = PAT.match(shellscript).group(1)
    command = get_command(open(shellscript))
    content =  template % {"server_name": server_name,
                      "command": command}
    open("./servers/%s.ini" % server_name, "w").write(content)
    
