This package provides a language server for Modula-3.

The server binary, as shipped, will run on linux/amd64 hardware. For Mac
and Windows installations you will have to rebuild from source.
See the m3-tools/langserver directory in the github repo and review
the readme.txt file there for more information.
After rebuilding the server, copy the resultant binary (m3_lang_server)
to the bin directory in this package's extension directory.
To locate the extension directory use one of the locations below.

Windows %USERPROFILE%\.vscode\extensions
macOS ~/.vscode/extensions
Linux ~/.vscode/extensions

The extension name will be infernus.modula-3-lang-server-[version]

[Modula-3](https://en.wikipedia.org/wiki/Modula-3) was [designed](https://doi.org/10.1145/142137.142141) in the late 1980s as a systems programming language based on Modula-2, which was designed by Niklaus Wirth, and Modula-2+, which was previously developed at Digital Equipment Corporation.  Its major implementation today is [Critical Mass Modula-3 (CM3)](https://github.com/modula3/cm3).

Quake is a small structured scripting language designed for writing makefiles for CM3 projects.

Visit the [CM3 repository](https://github.com/modula3/cm3) on GitHub to learn more.
