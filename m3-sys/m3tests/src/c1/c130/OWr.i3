(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE OWr;

(* An OWr.T is a sink of charaters.  Its destination can be other writers 
   (it is then called a filter) or a file or a reader, for example.

   Each writer has a current position; each character sent to the writer 
   goes to the current position and that position is advanced by one. 
   Some writers allow the user to change the current position to an arbitrary 
   place:  their seekable method returns TRUE.  Other don't allow that, their
   seekable method returns FALSE and the seek method raises the exception 
   Unseekable.  Initially, the position is 0.

   Normally, an OWr.T is open when created; after it is closed (by the close
   method), all the methods but close and isClosed raise the exception Closed. 
*)
   

IMPORT Text;

EXCEPTION 
  Closed;
  Unseekable;
 
TYPE
  T = OBJECT METHODS
        putChar   (ch: CHAR) RAISES {Closed};
        putString (READONLY a: ARRAY OF CHAR) RAISES {Closed};
        putText   (t: Text.T) RAISES {Closed} := PutText;

        preferredSize (): CARDINAL RAISES {Closed};
          (* return the optimal number of characters to give in one
             putString or putText call. *)

        seek (pos: CARDINAL) RAISES {Closed, Unseekable};
          (* change the current position of the writer to pos;
             raise Unseekable is the writer is not seekable *)
        flush () RAISES {Closed};
          (* ensure that the characters given to the put* methods are
             sent to the destination of the writer(s) and that 
             the destination is flushed *)
        close ();
          (* close the writer; calling close on a closed writer is a no-op *)

        isClosed (): BOOLEAN;
        isSeekable (): BOOLEAN RAISES {Closed}; END;

PROCEDURE PutText (self: T; t: Text.T) RAISES {Closed};
  (* use the putString method to send a Text.T to self *)

END OWr.
