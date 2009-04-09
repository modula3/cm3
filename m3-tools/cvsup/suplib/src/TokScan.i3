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
 * $Id: TokScan.i3,v 1.1.1.1 2009-04-09 17:02:01 jkrell Exp $ *)

(* The "TokScan" interface provides rudimentary facilities for scanning
   tokens from a line of text. *)

INTERFACE TokScan;

IMPORT IP, MD5Digest, Time, Word;

EXCEPTION Error(TEXT);

CONST
  Blanks = SET OF CHAR{' ', '\t', '\r', '\n'};

TYPE
  T = OBJECT METHODS
    next(VAR tok: TEXT): BOOLEAN RAISES {Error};

    getToken(what: TEXT := "token"): TEXT RAISES {Error};
    getChar(what: TEXT := "single-character token"): CHAR RAISES {Error};
    getInt(what: TEXT := "integer"; radix: [2..16] := 10): Word.T
      RAISES {Error};
    getTime(what: TEXT := "time"): Time.T RAISES {Error};
    getRCSDate(what: TEXT := "RCS date"): Time.T RAISES {Error};
    getMD5(what: TEXT := "MD5 checksum"): MD5Digest.T RAISES {Error};
    getEndpoint(what: TEXT := "IP endpoint"): IP.Endpoint RAISES {Error};
    getLiteral(what: TEXT) RAISES {Error};
    getFolded(what: TEXT) RAISES {Error};
    getEnd(what: TEXT := "end") RAISES {Error};

    getRest(): TEXT RAISES {Error};
  END;

(* A "TokScan.T", or token scanner, is an iterator that accesses
   successive tokens from a line of text.  The tokens are delimited by
   strings of one or more of the characters in "separators".

   The fundamental method is "next".  Each call to "next" passes
   back the next token, and returns "TRUE".  If there are no more
   tokens, "next" returns "FALSE".

   The remaining methods are convenience wrappers around "next".
   "getToken" returns the next token, or raises an exception if none is
   present.  The "what" argument is used to construct an error message,
   in the event that the token is not present.  For example, the default
   value of "what" produces the message "Missing token".

   "getChar" scans a single-character token.  It returns the token as
   a CHAR.  If it is not exactly 1 character in length, the exception
   is raised.

   "getInt" is like "getToken", except that it additionally parses the
   token as an unsigned integer.  It returns the numeric value of the
   integer.

   "getTime" scans a time and returns its value.  The format of a time
   value is defined to be whatever is produced by "EncodeTime".

   "getRCSDate" scans a date in RCS file format, and returns its value.

   "getEndpoint" scans an "IP.Endpoint", and returns its value.

   "getLiteral" scans the next token, and checks to make sure it is
   identical to the "what" parameter.  If it is not, an exception is
   raised.  "getFolded" is similar, but it uses a case-insensitive
   comparison.

   "getEnd" checks to make sure there are no more tokens to scan, and
   raises an exception if that is not the case.

   "getRest" returns all of the remaining unscanned text, after discarding
   leading separators. *)

PROCEDURE New(t: TEXT;
              READONLY separators: SET OF CHAR := Blanks;
	      emptyTokens := FALSE): T;
(* Creates a new token scanner.  If emptyTokens is "FALSE", then
   consecutive separator characters are treated as one.  Otherwise
   consecutive separators are interpreted as an empty token. *)

PROCEDURE NewDec(t: TEXT): T;
(* Creates a new token scanner which decodes escaped white space in
   its tokens. *)

(* This interface also provides the following useful procedures. *)

PROCEDURE AtoI(t: TEXT; what: TEXT := "integer"; radix: [2..16] := 10): Word.T
  RAISES {Error};
(* Converts the given text to an unsigned number. *)

PROCEDURE DecodeTime(text: TEXT): Time.T
  RAISES {Error};
(* Converts an encoded time to its value. *)

PROCEDURE EncodeEndpoint(READONLY ep: IP.Endpoint;
                         VAR toks: ARRAY [0..4] OF TEXT);
(* Converts an "IP.Endpoint" to 5 textual tokens. *)

PROCEDURE EncodeTime(time: Time.T): TEXT;
(* Converts a time value to a text string.  The time value may be negative. *)

PROCEDURE EqualFolded(a, b: TEXT): BOOLEAN;
(* Determines whether two text strings are the same, disregarding
   differences of letter case. *)

PROCEDURE ScanLeadingInt(t: TEXT; VAR pos: CARDINAL): Word.T;
(* Scans a decimal integer from "t" starting at "pos", and returns
   its value.  Updates "pos" to the position just beyond the integer. *)

PROCEDURE Trim(t: TEXT): TEXT;
(* Trims leading and trailing whitespace from "t" and returns the result. *)

END TokScan.
