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
 *
 * $Id: ProcessEnv.i3,v 1.1 2008-01-30 23:45:36 wagner Exp $ *)

(*---------------------------------------------------------------------------*)
INTERFACE ProcessEnv;

IMPORT TextTextTbl;

(*---------------------------------------------------------------------------*)
TYPE T = TextTextTbl.T;

(*---------------------------------------------------------------------------*)
PROCEDURE Current() : T;
  (* Get the complete environment of the current process. This will be the
     same data structure for every call, so it should be considered
     readonly. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Copy(env : T) : T;
  (* Create a copy of environment `env'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SystemRepr(env : T) : REF ARRAY OF TEXT;
  (* Return the system representation of an environment, that is, an
     array of TEXT entries of the form `name=value'. This can be
     passed as parameter to Process.Create(). *)

(*---------------------------------------------------------------------------*)
PROCEDURE Names(env : T) : REF ARRAY OF TEXT;
  (* Return an array of all names in `env'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Defined(env : T; name : TEXT) : BOOLEAN;
  (* <=> Variable `name' is defined in `env' . *)

(*---------------------------------------------------------------------------*)
PROCEDURE Value(env : T; name : TEXT) : TEXT;
  (* Return the value of `name' in `env'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Set(env : T; name : TEXT; value : TEXT);
  (* Set the value of `name' in `env' to `value'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Delete(env : T; name : TEXT);
  (* Delete the variable `name' form `env'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Add(env : T; env2 : T);
  (* Add all entries from  form `env2' to `env', overiding any already
     defined ones. *)

END ProcessEnv.
