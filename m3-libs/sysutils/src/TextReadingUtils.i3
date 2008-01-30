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
 * $Id$ *)

(*--------------------------------------------------------------------*)
INTERFACE TextReadingUtils;

IMPORT ASCII, Rd, TextSeq;
FROM Thread IMPORT Alerted;

(*--------------------------------------------------------------------*)
PROCEDURE GetString(rd : Rd.T) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted};
  (* skip everything until the next single or double quote, then
     read upto the following matching quote and return this text *)

(*--------------------------------------------------------------------*)
PROCEDURE GetToken(rd : Rd.T; skip := ASCII.Spaces; 
                   terminate := ASCII.Spaces) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted};
  (* get the next token, that is everything that is not considered
     to be ASCII white space *)

(*--------------------------------------------------------------------*)
PROCEDURE GetTokenOrString(rd : Rd.T; skip := ASCII.Spaces; 
                           terminate := ASCII.Spaces) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted};
  (* skip white space. If the next character is a single or a double
     quote, return the quoted text, otherwise return the next token *)

(*--------------------------------------------------------------------*)
PROCEDURE GetStringOrLine(rd : Rd.T) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted};
  (* skip white space. If the next character is a single or a double
     quote, return the quoted text, otherwise return the rest of
     the current line. *)

(*--------------------------------------------------------------------*)
PROCEDURE Tokenize(t : TEXT; sep := ASCII.Spaces) : TextSeq.T;
  (* tokenize the text `t' and return all separate tokens. *)

END TextReadingUtils.
