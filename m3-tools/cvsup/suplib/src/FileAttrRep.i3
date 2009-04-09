(* Copyright 1997-2003 John D. Polstra.
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

INTERFACE FileAttrRep;

IMPORT FileAttr, OSError, Pathname, Ustat;

REVEAL
  FileAttr.T <: Rep;

TYPE
  Rep = FileAttr.Public OBJECT
    mask := FileAttr.AttrTypes{};	(* Which fields are valid. *)
    stat: Ustat.struct_stat;
    linkTarget: TEXT;
  END;

PROCEDURE GetFlags(fa: FileAttr.T): INTEGER;
(* Return the "flags" attribute from "fa".  It is a checked runtime
   error if the attribute is unsupported. *)

PROCEDURE SetFlags(fa: FileAttr.T; flags: INTEGER);
(* Set the "flags" attribute  of "fa".  It is a checked runtime
   error if the attribute is unsupported. *)

PROCEDURE ChangeFileFlags(fa: FileAttr.T; path: Pathname.T; follow: BOOLEAN)
  RAISES {OSError.E};
(* Change the flags of the given pathname to the settings in "fa". *)

END FileAttrRep.
