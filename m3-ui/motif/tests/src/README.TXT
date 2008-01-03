---------------
Task: Hello
---------------
Make an X/Motif application which provides a "run" button.
When the button is pushed, "Hello, world" shows up in a textfield.

---------------------------
Implementation in Modula-3
[H. George]
---------------------------
Notice the following items:

1. Modules are used to isolate "unsafe" X/Motif code,
   and generally for implementation hiding.

2. Can handle XtVaSetValues-style functions (optional parms,
   and default values).

3. Typesafe, even for X/Motif work.  

4. Named parms make complex function calls more understandable.

5. Can do structural equivalence, so that we don't have
   to import the definition of Argv that RTArgs used --
   we can just restate it ourselves and LOOPHOLE.

6. Exception handling is available and "free".

7. Variables can be initialized.
