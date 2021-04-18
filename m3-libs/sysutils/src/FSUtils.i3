(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*--------------------------------------------------------------------------*)
INTERFACE FSUtils;

IMPORT Pathname, TextSeq;

(*--------------------------------------------------------------------------*)
EXCEPTION E(TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE Exists(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' exists *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is readable by this process *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsWritable(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is writable by this process *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsExecutable(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is executable by this process *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsDir(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is a directory *)

(*--------------------------------------------------------------------------*)
PROCEDURE IsFile(fn : Pathname.T) : BOOLEAN;
  (* <=> `fn' is an ordinary file *)

(*--------------------------------------------------------------------------*)
PROCEDURE MakeDir(path : Pathname.T);
  (* Build all directories in `path', if they do not exist, or crash. *)

(*--------------------------------------------------------------------------*)
PROCEDURE SubDirs(path : Pathname.T; relative := FALSE) : TextSeq.T
  RAISES {E};
  (* Return a list of all subdirectories. *)

(*--------------------------------------------------------------------------*)
PROCEDURE SubFiles(path : Pathname.T; relative := FALSE) : TextSeq.T
  RAISES {E};
  (* Return a list of all ordinary files in directory `path'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE RemoveFile(fn : Pathname.T);
  (* Remove file `fn' if it exists and crash in case of errors. *)

(*--------------------------------------------------------------------------*)
PROCEDURE RemoveDir(fn : Pathname.T);
  (* Remove directory `fn' if it exists and crash in case of errors. *)

(*--------------------------------------------------------------------------*)
PROCEDURE TouchFile(fn : Pathname.T);
  (* Touch the file `fn' and crash in case of errors. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Mkdir(path : Pathname.T) RAISES {E};
  (* Build all directories in `path', if they do not exist. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Rm(fn : Pathname.T) RAISES {E};
  (* Remove file `fn' if it exists. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Rmdir(fn : Pathname.T) RAISES {E};
  (* Remove directory `fn' if it exists. *)

(*--------------------------------------------------------------------------*)
PROCEDURE RmRec(fn : Pathname.T) RAISES {E};
  (* Remove directory `fn' and all its contents recursively. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Touch(fn : Pathname.T) RAISES {E};
  (* Touch the file `fn'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE CanonicalPathname(fn : Pathname.T) : Pathname.T RAISES {E};
  (* Return a unique absolute path for file `fn'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Cp(src, dest : Pathname.T) RAISES {E};
  (* Copy file `src' to file 'dest'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE FileContents(fn : Pathname.T) : TEXT RAISES {E};
  (* Return the contents of file `fn' as text. *)

(*---------------------------------------------------------------------------*)
PROCEDURE PutFile(fn : Pathname.T; data : TEXT) RAISES {E};
  (* Write `data' into file `fn'. Create or overwrite `fn' as needed. *)

(*---------------------------------------------------------------------------*)

END FSUtils.
