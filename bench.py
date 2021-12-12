import subprocess
import re

p = subprocess.run(["cabal", "run", "alltests"], capture_output=True, encoding="utf8")

for line in re.sub(r"Day(..).*?Finished", "Day\\1", p.stdout, flags=re.S).split('\n'):
    if "Day" in line:
        print(line)
