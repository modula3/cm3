
Abstract:

  These bindings support Modula-3 use of Motif.

For further information, contact:

  Harry George
  hgeorge@eskimo.com

Manifest:

  README.TXT          this file
  IntrinsicP.i3       Binding for IntrinsicP.h, CoreP.h
  Xmacros.i3/m3       Treatment for "Va" calls.
  Xm.i3/m3            Binding for Xm.h
  XmN.i3.m3           Binding for Xm "names"
  XmC.i3/m3           Binding for Xm "classes"
  Xm3.i3/m3           Collected bindings for motif widgets.
  ../test/            sample application

Notes.

  I built these using a perl script to partially translate from .h, then
  tidied up by hand.  These bindings are incomplete, but adequate to do
  many Motif applications.

  Xmacro was a place to do #define macros which were not addressed in
  the X11R4 bindings.  I used it to do variable length arguments.  I
  added an easy way to do LOOPHOLEs and XtStrings.  See sample app for
  uses.

