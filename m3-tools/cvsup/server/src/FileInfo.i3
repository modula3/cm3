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

INTERFACE FileInfo;

IMPORT FileAttr, Pathname;

CONST
  Brand = "FileInfo";

TYPE
  T = OBJECT
    name: Pathname.T;
    type: Type;
    attr: FileAttr.T;     (* For main or only version of file *)
  END;

  Type = { DirDown, Live, Dead, DirUp };

(* A "FileInfo.T" contains information about a file from a CVS
   repository.  It combines information from the main repository and
   from the Attic.

   A type of "Live" means that the file exists in the main repository.
   "Dead" means it exists in the Attic.  If a file exists in both places,
   it is treated as "Live", and the Attic version is ignored.

   "attr" contains the file's attributes. *)

PROCEDURE Compare(a, b: T): [-1..1];
(* Compares the names lexicographically, except that a "T" whose type
   is "Type.DirUp" is greater than anything of which it is a proper
   prefix. *)

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Equivalent to "Compare(a, b) = 0". *)

PROCEDURE IsDir(fi: T): BOOLEAN;
(* Returns TRUE iff "fi.type" is "Type.DirDown" or "Type.DirUp". *)

END FileInfo.
