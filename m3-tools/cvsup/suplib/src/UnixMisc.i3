(* Copyright 1996-2003 John D. Polstra.
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
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
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
 *
 * $Id$ *)

(* The "UnixMisc" interface provides miscellaneous Unix-related services. *)

INTERFACE UnixMisc;

IMPORT Ctypes, File, IP, OSError, Pathname, Ustat, Utypes, Word;

TYPE
  SignalHandler = PROCEDURE (sig: Ctypes.int);

PROCEDURE AppendAlways(file: File.T)
  RAISES {OSError.E};
(* Mark the underlying file descriptor so that writes always append to
   the end of the file. *)

PROCEDURE FStat(file: File.T;
                VAR statbuf: Ustat.struct_stat)
  RAISES {OSError.E};
(* Gets the status of the given file. *)

PROCEDURE GetHostAddrs(host: TEXT): REF ARRAY OF IP.Address;
(* Returns an array containing all of a host's IP addresses.  Returns
   NIL if the name lookup fails.  Not re-entrant. *)

PROCEDURE GetHostName(): TEXT
  RAISES {OSError.E};
(* Returns the host name as obtained from "Unix.gethostname()". *)

PROCEDURE GetLogin(): TEXT;
(* Returns the user's login name, or "NIL" if it is not known. *)

PROCEDURE GetMode(path: Pathname.T): Utypes.mode_t
  RAISES {OSError.E};
(* Returns the file mode for the given file. *)

PROCEDURE GetUmask(): Utypes.mode_t;
(* Returns the umask setting. *)

PROCEDURE MapFile(p: Pathname.T;
                  VAR statbuf: Ustat.struct_stat): ADDRESS
  RAISES {OSError.E};
(* Maps the given file into memory with a read-only shared mapping.
   Fills in "statbuf" with the status information of the file.
   Returns the address of the mapped region.  It is a checked runtime
   error to attempt to map anything but a regular file. *)

PROCEDURE MaskMode(mode: Utypes.mode_t; umask := -1): Utypes.mode_t;
(* Returns the given mode, as modified by the "umask" value.  If it
   is defaulted, then the program's umask setting is used. *)

PROCEDURE ReadLink(path: Pathname.T): TEXT
  RAISES {OSError.E};
(* Reads the given symbolic link and returns it as text. *)

<* EXTERNAL "setsid" *>
PROCEDURE SetSID(): Utypes.pid_t;
(* Set session-ID. *)

<* EXTERNAL "UnixMiscSigIsIgnored" *>
PROCEDURE SigIsIgnored(sig: Ctypes.int): BOOLEAN;
(* Reports whether a given signal is currently set up to be ignored. *)

<* EXTERNAL "UnixMiscSignal" *>
PROCEDURE Signal(sig: Ctypes.int;
                 func: SignalHandler): SignalHandler;
(* Like the standard C signal() function, except it disables thread
   scheduling when the handler is executing. *)

PROCEDURE Stat(path: Pathname.T;
               VAR statbuf: Ustat.struct_stat)
  RAISES {OSError.E};
(* Gets the status of the given file. *)

PROCEDURE Unmap(adr: ADDRESS; size: Word.T) RAISES {OSError.E};
(* Unmaps the given address range. *)

END UnixMisc.
