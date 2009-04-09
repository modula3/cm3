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
 * $Id: FileAttrOS.m3,v 1.1.1.1 2009-04-09 17:02:02 jkrell Exp $ *)

UNSAFE MODULE FileAttrOS EXPORTS FileAttr, FileAttrRep, FileAttrOS;

IMPORT CText, OSError, OSErrorPosix, Pathname;

(* On platforms that do not support "flags", the following procedures
   should simply cause an assertion failure. *)

PROCEDURE ChangeFileFlags(fa: T;
                          path: Pathname.T;
			  <*UNUSED*> follow: BOOLEAN)
  RAISES {OSError.E} =
  VAR
    pathStr := CText.SharedTtoS(path);
  BEGIN
    TRY
      IF chflags(pathStr, fa.stat.st_flags) = -1 THEN
	OSErrorPosix.Raise();
      END;
    FINALLY
      CText.FreeSharedS(path, pathStr);
    END;
  END ChangeFileFlags;

PROCEDURE GetFlags(fa: T): INTEGER =
  BEGIN
    RETURN fa.stat.st_flags;
  END GetFlags;

PROCEDURE SetFlags(fa: T; flags: INTEGER) =
  BEGIN
    fa.stat.st_flags := flags;
  END SetFlags;

BEGIN
  Supported := SupportInfo{ AttrTypes{}, .. };

  Supported[FileType.File] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.Size, AttrType.ModTime, AttrType.Flags, AttrType.LinkCount,
    AttrType.Dev, AttrType.Inode };

  Supported[FileType.Directory] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.Flags };

  Supported[FileType.CharDevice] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.RDev, AttrType.Flags, AttrType.LinkCount, AttrType.Dev,
    AttrType.Inode };

  Supported[FileType.BlockDevice] := AttrTypes{
    AttrType.FileType, AttrType.Owner, AttrType.Group, AttrType.Mode,
    AttrType.RDev, AttrType.Flags, AttrType.LinkCount, AttrType.Dev,
    AttrType.Inode };

  Supported[FileType.SymLink] := AttrTypes{
    AttrType.FileType, AttrType.LinkTarget };
END FileAttrOS.
