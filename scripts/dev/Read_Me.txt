Read_Me:
-------

Proposed "organization" of "scripts" folder.--Randy Coleburn, 10/29/2009

scripts
+---doc = documentation for stuff in the "scripts" folder tree
+---dev = scripts used for system development, maintenance, admin
+   +---windows = dev scripts written for Microsoft Windows (BAT, CMD)
+   +---posix = dev scripts written for unix shell (sh, bash, etc.)
+   \---python = dev scripts written in python
+---install = scripts that should be put in "bin" folder of target install
+   +---common = scripts common to all target platforms
+   +---windows = BAT/CMD scripts for Microsoft Windows
+   +---posix = shell scripts for POSIX platforms (sh, bash, etc.)
+   +---python = scripts written in python (applicable to any platform with python)
+   \---XXX = scripts for target platform XXX (XXX is name of platform)
+             (assuming XXX needs something special not covered above)
\---test = scripts used for regression testing
    +---windows = test scripts written for Microsoft Windows (BAT, CMD)
    +---posix = test scripts written for unix shell (sh, bash, etc.)
    \---python = test scripts written in python
