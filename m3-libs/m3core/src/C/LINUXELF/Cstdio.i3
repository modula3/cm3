(* Copyright (C) 1995, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Feb 27 08:51:20 PST 1995 by kalsow    *)
(*      modified on Thu May  6 09:18:19 PDT 1993 by muller    *)

INTERFACE Cstdio;

FROM Ctypes IMPORT char, int, long, char_star, unsigned_short;

CONST 
  NIOBRW = 3;
  IOEOF  = 8_20;

TYPE (* The C declarations prefix these fields with "_" *)
  FILE = RECORD 
    flags: int;  (* High-order word is _IO_MAGIC; rest is flags. *)
    (* C #define's "_IO_file_flags" to be "flags" *)

    (* The following pointers correspond to the C++ streambuf protocol. *)
    IO_read_ptr    : char_star; (* Current read pointer *)
    IO_read_end    : char_star; (* End of get area. *)
    IO_read_base   : char_star; (* Start of putback+get area. *)
    IO_write_base  : char_star; (* Start of put area. *)
    IO_write_ptr   : char_star; (* Current put pointer. *)
    IO_write_end   : char_star; (* End of put area. *)
    IO_buf_base    : char_star; (* Start of reserve area. *)
    IO_buf_end     : char_star; (* End of reserve area. *)

    (* The following fields are used to support backing up and undo. *)
    IO_save_base   : char_star; (* Pointer to start of non-current get area. *)
    IO_backup_base : char_star; (* Pointer to first valid char in backup *)
    IO_save_end    : char_star; (* Pointer to end of non-current get area. *)

    markers : IO_marker_star;
    chain   : FILE_star;
    jumps   : IO_jump_t_star; (* Jump table *)
    fileno  : int;
    blksize : int;
    offset  : long;  (* _IO_off_t is same?? *)
  
    (*** #define __HAVE_COLUMN (* temporary *)  ***)
    (* 1+column number of pbase(); 0 is unknown. *)
    cur_column : unsigned_short;
    unused     : char;
    shortbuf   : ARRAY [0..0] OF char;
  
    (*  char* _save_gptr;  char* _save_egptr; *)
  END;

  FILE_star = UNTRACED REF FILE;
  IO_jump_t_star = ADDRESS;  (* UNTRACED REF IO_jump_t ??*)
  IO_marker_star = ADDRESS;  (* UNTRACED REF IO_marker ??*)

VAR iob: ARRAY [0..NIOBRW-1] OF FILE_star;

<*EXTERNAL "_IO_stdin_"*> VAR stdin_file: FILE;
<*EXTERNAL "_IO_stdout_"*> VAR stdout_file: FILE;
<*EXTERNAL "_IO_stderr_"*> VAR stderr_file: FILE;

<*EXTERNAL feof*>      PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL getc*>      PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL ungetc*>    PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL putc*>      PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL fflush*>    PROCEDURE fflush (f: FILE_star): int;

END Cstdio.
