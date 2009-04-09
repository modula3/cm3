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

INTERFACE Logger;

TYPE
  T <: ROOT;

  Priority = {
    Emerg,
    Alert,
    Crit,	(* Fatal error. *)
    Err,	(* Error, but program can proceed. *)
    Warning,
    Notice,
    Info,
    Debug
  };

PROCEDURE Put(logger: T;
              priority: Priority;
	      msg: TEXT);

PROCEDURE Indent(logger: T);

PROCEDURE Exdent(logger: T);

PROCEDURE Count(logger: T; priority: Priority): CARDINAL;
(* Returns the number of messages logged with a severity of "priority"
   or worse. *)

PROCEDURE CountEqual(logger: T; priority: Priority): CARDINAL;
(* Returns the number of messages logged with a severity of exactly 
   "priority". *)

PROCEDURE Close(logger: T);

PROCEDURE Emerg(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Emerg, msg)". *)

PROCEDURE Alert(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Alert, msg)". *)

PROCEDURE Crit(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Crit, msg)". *)

PROCEDURE Err(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Err, msg)". *)

PROCEDURE Warning(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Warning, msg)". *)

PROCEDURE Notice(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Notice, msg)". *)

PROCEDURE Info(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Info, msg)". *)

PROCEDURE Debug(logger: T; msg: TEXT);
(* Shorthand for "Put(logger, Priority.Debug, msg)". *)

END Logger.
