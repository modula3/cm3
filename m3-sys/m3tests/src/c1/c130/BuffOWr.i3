(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE BuffOWr;

IMPORT OWr;

(* Buffered writers are filters that have an internal buffer. The characters
   sent to them are inserted in the buffer.  Some of the characters in the
   buffer may be sent to the underlying writer anytime.  All of the characters in
   the buffer are sent to the underlying writer when the buffer is full or 
   when some condition occurs.

   Flushing a buffered writer sends the characters sitting in the buffer to
   the underlying writer and flushes it.

   If the size of the buffer is not specified in create/init call, it
   is determined from the underlying writer.  *)


TYPE
  T <: OWr.T;


  (* A block buffered writer has no additional condition to send its characters
     to the underlying writer *)

  B <: T OBJECT METHODS
          new (wr: OWr.T; size: CARDINAL := 0): B;
            (* create (self = NIL) or initialize (self # NIL) a block
               buffered writer with destination 'wr'.  If 'size' is 0, the size
               of the buffer is the preferredSize of the underlying writer;
               otherwise, size is used *)
       END;


  (* A line buffered writer sends its characters to the underlying writer
     when it receives a '\n' *)

  L <: T OBJECT METHODS
          new (wr: OWr.T; size: CARDINAL := 0): L;
            (* create (self = NIL) or initialize (self # NIL) a line
               buffered writer with destination 'wr'.  If 'size' is 0, the size
               of the buffer is the preferredSize of the underlying writer;
               otherwise, size is used *)
       END;

END BuffOWr.
