(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Aug 28 10:13:45 PDT 1994 by mhb                      *)
(*      modified on Fri Jun 11 23:23:22 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:08:06 PDT 1992 by muller                   *)
(*      modified on Fri Mar 20 22:37:08     1992 by steveg                   *)
(*      modified on Fri Feb  8 15:32:36 PST 1991 by brooks                   *)
<* PRAGMA LL                                                                 *>

(* A "TypescriptVBT" is a subtype of "TextEditVBT", with additional
   features to make it serve as a ``glass teletype'' with a memory.

   Abstractly, a typescript contains

| reader(v)           `an intermittent, unseekable reader`
| writer(v)           `a buffered, unseekable writer`
| readingThread(v)    `a thread`
   
   "reader(v)" provides the client with input that the user typed.
   "writer(v)" is used to display output. The reader and writer are
   paired such that the writer is flushed whenever a seek blocks on
   the reader.  The writer is not flushed at every newline.

   All input to the typescript, once it has been read, and all output,
   become part of the {\em history} of the typescript, and is not
   modifiable; it remains until the client deletes it by calling
   "ClearHistory".  Selections that lie fully or partially within the
   history region are never ``replace-mode'' selections (see
   Section~\ref{ReplaceMode}, page~\pageref{ReplaceMode}).  Any
   attempt to type or insert text in the history region becomes an
   insertion at the end of the typescript instead.

   "readingThread(v)" is initially "NIL". When a client reads from
   "v", "readingThread(v)" is set to "Thread.Self()". The
   "handleInterrupt" method (see below) alerts "readingThread(v)".
   This is useful when the reading thread is blocked waiting for
   input.

   A typescript's textport, "v.tp", must be of type
   "TypescriptVBT.Port" (which is a subtype of "TextPort.T").  The
   textport's "returnAction" method makes the text of the current
   type-in region available to the reader and no longer editable. The
   textport's "setReadOnly" method is a no-op.

   Typescripts do not allow the use of Undo and Redo.

*)

INTERFACE TypescriptVBT;

IMPORT Rd, TextEditVBT, TextPort, VBT, Wr, Thread;

TYPE
  T <: Public;
  Public = TextEditVBT.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init            (scrollable := TRUE): T;
             interrupt       (time: VBT.TimeStamp);
             handleInterrupt (time: VBT.TimeStamp);
             terminate       ();
             setThread       (thread: Thread.T := NIL);
           END;
  Port <: TextPort.T;

(* The call "v.init()" initializes "v" as an empty typescript.

   It is a checked runtime error if "v.tp" is "NIL" or is not of type
   "TypescriptVBT.Port", which is a subtype of "TextPort.T".

   The call "v.interrupt(time)" simulates an interrupt by
   flushing any pending type-in, writing the characters "^C", and then
   calling "v.handleInterrupt(time)". 

   The call "v.handleInterrupt(time)" alerts "readingThread(v)".

   After "v.terminate()" is called, subsequent attempts to read from
   "v" will causes it to report end of file, and "v" becomes
   unresponsive to further user input, although it will continue to
   display output written to its output stream.  This is appropriate
   when "v" is being discarded.

   The call "v.setThread(thread)" changes "readingThread(v)".  This
   can be used to protect "Thread.Self()" from being alerted after it
   has finished reading from "reader(v)".  Subsequent reads on
   "reader(v)" will reset the "readingThread(v)" to "Thread.Self()".*)

TYPE
  Reader <: PublicReader;
  PublicReader = Rd.T OBJECT METHODS typescript (): T END;

  Writer <: PublicWriter;
  PublicWriter = Wr.T OBJECT METHODS typescript (): T END;

PROCEDURE GetRd (v: T): Reader;
(* Get the input stream for "v". By definition,
| GetRd(v).typescript() = v
*)

PROCEDURE GetWr (v: T): Writer;
(* Get the output stream for "v". By definition,
| GetWr(v).typescript() = v
*)

PROCEDURE GetHistory (v: T): TEXT; <* LL <= VBT.mu *>
(* Return the ``history'' text of "v". *)

PROCEDURE ClearHistory (v: T);  <*  LL <= VBT.mu *>
(* Clear the ``history'' text of "v". *)

END TypescriptVBT.
