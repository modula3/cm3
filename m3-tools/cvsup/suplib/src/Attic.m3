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
 *)

MODULE Attic;

IMPORT
  File, FileAttr, FileRd, FS, OSError, Pathname, RCSError, RCSFile,
  SupMisc, UnixMisc, Ustat;

PROCEDURE FileAttrFromPathname(VAR path: Pathname.T;
                               follow: BOOLEAN): FileAttr.T
  RAISES {OSError.E} =
  VAR
    origPath := path;
  BEGIN
    TRY
      RETURN FileAttr.FromPathname(path, follow);
    EXCEPT OSError.E(l1) =>  (* Try the attic. *)
      TRY
	path := SupMisc.AtticName(path);
	RETURN FileAttr.FromPathname(path, follow)
      EXCEPT OSError.E =>
	(* Restore the path, and re-raise the original exception. *)
	path := origPath;
	RAISE OSError.E(l1);
      END;
    END;
  END FileAttrFromPathname;

PROCEDURE FileRdOpen(VAR path: Pathname.T): FileRd.T
  RAISES {OSError.E} =
  VAR
    origPath := path;
  BEGIN
    TRY
      RETURN FileRd.Open(path);
    EXCEPT OSError.E(l1) =>  (* Try the attic. *)
      TRY
	path := SupMisc.AtticName(path);
	RETURN FileRd.Open(path)
      EXCEPT OSError.E =>
	(* Restore the path, and re-raise the original exception. *)
	path := origPath;
	RAISE OSError.E(l1);
      END;
    END;
  END FileRdOpen;

PROCEDURE FSOpenFileReadonly(VAR path: Pathname.T): File.T
  RAISES {OSError.E} =
  VAR
    origPath := path;
  BEGIN
    TRY
      RETURN FS.OpenFileReadonly(path);
    EXCEPT OSError.E(l1) =>  (* Try the attic. *)
      TRY
	path := SupMisc.AtticName(path);
	RETURN FS.OpenFileReadonly(path)
      EXCEPT OSError.E =>
	(* Restore the path, and re-raise the original exception. *)
	path := origPath;
	RAISE OSError.E(l1);
      END;
    END;
  END FSOpenFileReadonly;

PROCEDURE FSStatus(VAR path: Pathname.T): File.Status
  RAISES {OSError.E} =
  VAR
    origPath := path;
  BEGIN
    TRY
      RETURN FS.Status(path);
    EXCEPT OSError.E(l1) =>  (* Try the attic. *)
      TRY
	path := SupMisc.AtticName(path);
	RETURN FS.Status(path)
      EXCEPT OSError.E =>
	(* Restore the path, and re-raise the original exception. *)
	path := origPath;
	RAISE OSError.E(l1);
      END;
    END;
  END FSStatus;

PROCEDURE RCSFileOpenReadonly(VAR path: Pathname.T): RCSFile.T
  RAISES {OSError.E, RCSError.E} =
  VAR
    origPath := path;
  BEGIN
    TRY
      RETURN RCSFile.OpenReadonly(path);
    EXCEPT OSError.E(l1) =>  (* Try the attic. *)
      TRY
	path := SupMisc.AtticName(path);
	RETURN RCSFile.OpenReadonly(path)
      EXCEPT OSError.E =>
	(* Restore the path, and re-raise the original exception. *)
	path := origPath;
	RAISE OSError.E(l1);
      END;
    END;
  END RCSFileOpenReadonly;

PROCEDURE Stat(VAR path: Pathname.T; VAR statbuf: Ustat.struct_stat)
  RAISES {OSError.E} =
  VAR
    origPath := path;
  BEGIN
    TRY
      UnixMisc.Stat(path, statbuf);
    EXCEPT OSError.E(l1) =>  (* Try the attic. *)
      TRY
	path := SupMisc.AtticName(path);
	UnixMisc.Stat(path, statbuf);
      EXCEPT OSError.E =>
	(* Restore the path, and re-raise the original exception. *)
	path := origPath;
	RAISE OSError.E(l1);
      END;
    END;
  END Stat;

BEGIN
END Attic.
