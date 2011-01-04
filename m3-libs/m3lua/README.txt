M3LUA - Modula-3 bindings for the Lua scripting language.

These bindings contain a reimplementation of the C-API for the Lua
scripting language, along with an alternate object oriented API.  
This will allow a program written in Modula-3 to be an engine, for 
which lua scripts will invoke functions within the engine.  
A Modula-3 program will register functions, and variables
with the lua library when it wants to run a lua script.  The lua
script can invoke those functions and use/set variables.  

One typical use of a lua script would be as configuration files.
For example, one could have an administrator configuration in
/etc/myapp/app.config, then another in the users home directory
${HOME}/.myapp/user.config.  If the program was complex enough
a project configuration could also be used from the current directory.

This source tree contains the source code for the bindings in 'src',
and an set of checks code in 'check' which implements a very simple
lua interpreter.  If you decide to use the Makefile by invoking 'make'
it will compile both the lua bindings and the check code.

Alternatively, one may choose to manually descend into the src directory,
and invoke the modula-3 compiler from there.


