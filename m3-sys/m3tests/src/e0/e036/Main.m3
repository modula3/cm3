(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
Return-Path: meehan@src.dec.com
Received: by jumbo.pa.dec.com; id AA02130; Tue, 11 Feb 92 23:59:39 -0800
From: meehan (Jim Meehan)
Message-Id: <9202120759.AA02130@jumbo.pa.dec.com>
Date: Tue, 11 Feb 92 23:59:27 PST
To: kalsow, muller
Subject: M3 compiler crash


~meehan/M3core contains a core dump  that occurred when the compiler
encountered the enclosed file.  (It's probably the typo "T <; Public"
with a semicolon instead of a colon.)  Delete it when you're done
with it.



(* Copyright 1992 Digital Equipment Corporation.                 *)
(* Distributed only by permission.                               *)
(* Last modified on Tue Feb 11 23:11:53 PST 1992 by meehan       *)

INTERFACE FormsEditVBT;

IMPORT Filter, FormsVBT;

(*
    A FormsEditVBT is a Filter whose child is a FormsVBT. When you
    initialize a FormsEditVBT with a file or other description, you get
    a second FormsVBT, the editor, which can installed and used to
    edit the filter's child.

    The stand-alone application called 'miniformsedit' is the major
    client of this interface, but any program that constructs FormsVBT's
    can do so through this interface and get an editor that can change
    the form dynamically.  This could be helpful during the development
    of an application, where changes can be made as the application
    is running.  Obviously, you could do major damage to the application
    (by removing named VBTs, for instance), but having an online editor
    for changing colors and shapes, for example, could be valuable.

    When the editor parses a new description, it constructs a new
    FormsVBT and replaces the filter's child with it, discarding the
    previous child, if any.  It also copies all the attached procedures
    from the old child to the new child.  Doing so will raise an error
    if the attachment fails, typically because the new form lacks a named
    VBT that the old form contained.  No other state-information is
    preserved, such as the contents of text-fields.
*)

TYPE

  Public =
    Filter.T OBJECT
    METHODS
      editor (): FormsVBT.T;
      init   (description: REFANY := NIL): T RAISES {FormsVBT.Error};
      initFromFile (filename: TEXT): T RAISES {FormsVBT.Error};
    END;
    
  T <; Public;

(* 
    The init and initFromFile methods are nearly the same as those in 
    the FormsVBT interface. The "raw" parameter is not supported, and
    the default description for init is NIL, which causes a "dummy"
    form to be used.
*)

END FormsEditVBT.



Here's the transcript:

myrtle 139> m3make
rm: .makefile.1 not removed
m3 -w1 -make -why -g -D.:../src:../src.editor:../src.editvbt:../../lego/src -L.:../mips:../mips.editvbt:../../lego/mips -o miniformsedit ./MiniFormsEdit.m3 -lm3miniformsvbt  -lm3formseditvbt  -lm3lego  -lm3sx  -lm3vtext  -lm3mtext  -lm3glist  -lm3uprocess  -lm3rdwrutils  -lm3color  -lm3rsrc  -lm3ui -lm3X11R4 -lX11
new source -> compile ./MiniFormsEdit.m3
"../src.editvbt/FormsEditVBT.i3", line 43: syntax error: missing '=' or '<:'
"../src.editvbt/FormsEditVBT.i3", line 52: syntax error: missing ';'
M3 runtime error: Segmentation violation - possible attempt to dereference NIL


Fatal Error: program "/proj/m3/lib.mips/m3compiler" got fatal signal 3

*** Error code 255

Stop.
