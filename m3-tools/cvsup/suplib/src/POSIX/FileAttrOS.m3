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

UNSAFE MODULE FileAttrOS EXPORTS FileAttr, FileAttrRep;

IMPORT Pathname;

(* The procedures in this module should never be called. *)

PROCEDURE ChangeFileFlags(<*UNUSED*> fa: T;
                          <*UNUSED*> path: Pathname.T;
			  <*UNUSED*> follow: BOOLEAN) =
  BEGIN
    <* ASSERT FALSE *>
  END ChangeFileFlags;

PROCEDURE GetFlags(<*UNUSED*> fa: T): INTEGER =
  BEGIN
    <* ASSERT FALSE *>
  END GetFlags;

PROCEDURE SetFlags(<*UNUSED*> fa: T;
                   <*UNUSED*> flags: INTEGER) =
  BEGIN
    <* ASSERT FALSE *>
  END SetFlags;

BEGIN
  Supported := SupportInfo{ AttrTypes{}, .. };

  Supported[FileType.File] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.Size, AttrType.ModTime, AttrType.LinkCount, AttrType.Dev,
    AttrType.Inode };

  Supported[FileType.Directory] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode };

  Supported[FileType.CharDevice] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.RDev, AttrType.LinkCount, AttrType.Dev, AttrType.Inode };

  Supported[FileType.BlockDevice] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.RDev, AttrType.LinkCount, AttrType.Dev, AttrType.Inode };

  Supported[FileType.SymLink] := AttrTypes{
    AttrType.FileType, AttrType.LinkTarget };
END FileAttrOS.
