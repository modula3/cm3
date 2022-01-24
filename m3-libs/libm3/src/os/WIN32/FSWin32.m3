(* Copyright (C) 1993, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.        *)
(*                                                          *)
(* Last modified on Thu Aug 31 14:02:00 PDT 1995 by steveg  *)
(*      modified on Wed Nov 30 13:44:49 PST 1994 by kalsow  *)
(*      modified on Fri Feb 18 10:45:30 PST 1994 by mcjones *)
(*      modified on Fri May  7 22:56:07 PDT 1993 by mjordan *)

UNSAFE MODULE FSWin32 EXPORTS FS;

IMPORT Ctypes, File, FileWin32, M3toC, OSError, OSErrorWin32, OSWin32,
  Pathname, RegularFile, Text, Time, TimeWin32, WinBase, WinDef,
  WinError, WinNT, Word, Long;

CONST
  False: WinDef.BOOL = 0;
  True: WinDef.BOOL = 1;

EXCEPTION InternalError; <* FATAL InternalError *>

PROCEDURE GetAbsolutePathname(p: Pathname.T): Pathname.T
  RAISES {OSError.E} =
  VAR
    fname : Ctypes.char_star := M3toC.SharedTtoS(p);
    n     : WinDef.DWORD;
    chars : ARRAY [0..63] OF CHAR;
  BEGIN
    n := GetAbsPath(p, fname, chars);
    IF n < NUMBER(chars) THEN
      M3toC.FreeSharedS(p, fname);
      RETURN Text.FromChars(SUBARRAY(chars, 0, n))
    END;
    WITH refChars = NEW(REF ARRAY OF CHAR, n + 1) DO
      n := GetAbsPath(p, fname, refChars^);
      M3toC.FreeSharedS(p, fname);
      IF n > NUMBER(refChars^) THEN RAISE InternalError END;
      RETURN Text.FromChars(SUBARRAY(refChars^, 0, n))
    END;
  END GetAbsolutePathname;

PROCEDURE GetAbsPath(p: Pathname.T;  fname: Ctypes.char_star;
                     VAR chars: ARRAY OF CHAR): WinDef.DWORD
  RAISES {OSError.E} =
  VAR filePart: WinNT.LPSTR;  n: WinDef.DWORD;
  BEGIN
    n := WinBase.GetFullPathName(
           lpFileName := fname, nBufferLength := NUMBER(chars),
           lpBuffer := ADR(chars[0]), lpFilePart := ADR(filePart));
    IF n = 0 THEN Fail(p, fname); END;
    RETURN n
  END GetAbsPath;

TYPE ABD = ARRAY BOOLEAN OF WinDef.DWORD;

VAR createMode := ARRAY CreateOption OF ABD{
  (* truncate =            FALSE                    TRUE            *)
  (* Never  *) ABD{WinBase.OPEN_EXISTING, WinBase.TRUNCATE_EXISTING},
  (* Ok     *) ABD{WinBase.OPEN_ALWAYS,     WinBase.CREATE_ALWAYS    },
  (* Always *) ABD{WinBase.CREATE_NEW,      WinBase.CREATE_NEW       }
  };

PROCEDURE OpenFile(
    p: Pathname.T;
    truncate: BOOLEAN := TRUE; 
    create: CreateOption := CreateOption.Ok;
    template: File.T := NIL; 
    accessOption: AccessOption := AccessOption.Default)
  : File.T RAISES {OSError.E} =
  VAR
    attrs: WinDef.DWORD;
    handle, handleTemplate: WinNT.HANDLE;
    rsd: REF ARRAY OF WinDef.BYTE;
    sd: ARRAY [0..WinNT.SECURITY_DESCRIPTOR_MIN_LENGTH-1] OF WinDef.BYTE;
    acl: ARRAY [0..100] OF WinDef.BYTE;
    sid: ARRAY [0..100] OF WinDef.BYTE;
    nSid: WinDef.DWORD := BYTESIZE(sid);
    user, domain: ARRAY [0..80-1] OF CHAR;
    nUser: WinDef.DWORD := NUMBER(user);
    nDomain: WinDef.DWORD := NUMBER(domain);
    use: WinNT.SID_NAME_USE;
    sa: WinBase.SECURITY_ATTRIBUTES;
    lpsa: WinBase.LPSECURITY_ATTRIBUTES;
  BEGIN
    IF template # NIL THEN
      handleTemplate := template.handle;
      attrs := GetFileAttributes(handleTemplate);
      rsd := GetFileSecurityDescriptor(p);
      IF (rsd = NIL) OR (NUMBER (rsd^) < 1) THEN
        (* we must be on Win95... *)
        handleTemplate := NIL;
        attrs := WinNT.FILE_ATTRIBUTE_NORMAL;
        lpsa := NIL;
      ELSE
        sa.nLength := BYTESIZE(sa);
        sa.lpSecurityDescriptor := ADR(rsd[0]);
        sa.bInheritHandle := False;
        lpsa := ADR(sa)
      END;
    ELSE
      handleTemplate := NIL;
      IF OSWin32.Win95() AND accessOption = AccessOption.OnlyOwnerCanRead THEN
        (* No "owner" under Win95 - WinBase.InitializeSecurityDescriptor
           not implemented *)
        accessOption := AccessOption.Default;
      END;

      CASE accessOption OF
      | AccessOption.OnlyOwnerCanRead =>
        IF WinBase.InitializeSecurityDescriptor(
             pSecurityDescriptor := ADR(sd),
             dwRevision := WinNT.SECURITY_DESCRIPTOR_REVISION) = False THEN
          OSErrorWin32.Raise()
        END;
        IF WinBase.InitializeAcl(
             pAcl := ADR(acl),
             nAclLength := BYTESIZE(acl),
             dwAclRevision := WinNT.ACL_REVISION) = False THEN
          OSErrorWin32.Raise()
        END;
        IF WinBase.GetUserName(
            lpBuffer := ADR(user[0]),
            nSize := ADR(nUser)) = False THEN
          OSErrorWin32.Raise()
        END;
        <* ASSERT nUser <= NUMBER(user) *>
        IF WinBase.LookupAccountName(
            lpSystemName := NIL, (* local system *)
            lpAccountName := ADR(user[0]),
            Sid := ADR(sid),
            cbSid := ADR(nSid),
            ReferencedDomainName := ADR(domain[0]),
            cbReferencedDomainName := ADR(nDomain),
            peUse := ADR(use)) = False THEN
          OSErrorWin32.Raise()
        END;
        <* ASSERT nSid <= BYTESIZE(sid) *>
        IF WinBase.AddAccessAllowedAce(
             pAcl := ADR(acl),
             dwAceRevision := WinNT.ACL_REVISION,
             AccessMask := WinNT.GENERIC_ALL,
             pSid := ADR(sid)) = False THEN
          OSErrorWin32.Raise()
        END;
        IF WinBase.SetSecurityDescriptorDacl(
             pSecurityDescriptor := ADR(sd),
             bDaclPresent := True,
             pDacl := ADR(acl),
             bDaclDefaulted := False) = False
        THEN OSErrorWin32.Raise()
        END;
      | AccessOption.ReadOnly => attrs := WinNT.FILE_ATTRIBUTE_READONLY;
      | AccessOption.Default => attrs := WinNT.FILE_ATTRIBUTE_NORMAL
      END;
      lpsa := NIL
    END;
    (* I believe the only reason for passing a non-NIL "hTemplate" to
       "CreateFile" is to supply OS/2-style ``extended attributes''
       for the file being created.  PMcJ 7/3/93 *)
    VAR fname := M3toC.SharedTtoS(p); BEGIN
      handle := WinBase.CreateFile(
        lpFileName := fname,
        dwDesiredAccess := WinNT.GENERIC_READ + WinNT.GENERIC_WRITE,
        dwShareMode := WinNT.FILE_SHARE_READ + WinNT.FILE_SHARE_WRITE,
        lpSecurityAttributes := lpsa,
        dwCreationDisposition := createMode[create, truncate],
        dwFlagsAndAttributes := attrs,
        hTemplateFile := handleTemplate);
      IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN
        Fail(p, fname);
      END;
      M3toC.FreeSharedS(p, fname);
    END;
    RETURN FileWin32.New(handle, FileWin32.ReadWrite)
  END OpenFile;

PROCEDURE GetFileAttributes(handle: WinNT.HANDLE): WinDef.DWORD
  RAISES {OSError.E} =
  VAR info: WinBase.BY_HANDLE_FILE_INFORMATION;
  BEGIN
    info.dwFileAttributes := 0;
    IF WinBase.GetFileInformationByHandle(handle, ADR(info)) = False THEN
      OSErrorWin32.Raise()
    END;
    RETURN info.dwFileAttributes;
  END GetFileAttributes;

PROCEDURE GetFileSecurityDescriptor(pn: Pathname.T): REF ARRAY OF WinDef.BYTE
  RAISES {OSError.E} =
  CONST Info = WinNT.OWNER_SECURITY_INFORMATION +
               WinNT.GROUP_SECURITY_INFORMATION +
               WinNT.DACL_SECURITY_INFORMATION +
               WinNT.SACL_SECURITY_INFORMATION;
  VAR
    rsd: REF ARRAY OF WinDef.BYTE;
    n, nNeeded: WinDef.DWORD;
    fname: Ctypes.char_star;
  BEGIN
    IF OSWin32.Win95() THEN RETURN NIL END; 
    (* WinBase.GetFileSecurity not implement in Win95 *)

    fname := M3toC.SharedTtoS(pn);
    n := 64;
    LOOP
      rsd := NEW(REF ARRAY OF WinDef.BYTE, n);
      IF WinBase.GetFileSecurity(
           lpFileName := fname,
           RequestedInformation := Info,
           pSecurityDescriptor := ADR(rsd[0]),
           nLength := n,
           lpnLengthNeeded := ADR(nNeeded)) = False THEN
        Fail(pn, fname);
      END;
      IF nNeeded = 0 THEN EXIT END;
      n := nNeeded;
    END;
    M3toC.FreeSharedS(pn, fname);

    RETURN rsd
  END GetFileSecurityDescriptor;

PROCEDURE OpenFileReadonly(p: Pathname.T): File.T RAISES {OSError.E}=
  VAR handle: WinNT.HANDLE;  fname := M3toC.SharedTtoS(p);
  BEGIN
    handle := WinBase.CreateFile(
      lpFileName := fname,
      dwDesiredAccess := WinNT.GENERIC_READ,
      dwShareMode :=  WinNT.FILE_SHARE_READ,
      lpSecurityAttributes := NIL,
      dwCreationDisposition := WinBase.OPEN_EXISTING,
      dwFlagsAndAttributes := 0,
      hTemplateFile := NIL);
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN
      Fail(p, fname);
    END;
    M3toC.FreeSharedS(p, fname);
    RETURN FileWin32.New(handle, FileWin32.Read)
  END OpenFileReadonly;

PROCEDURE CreateDirectory(p: Pathname.T) RAISES {OSError.E}=
  VAR sa := WinBase.SECURITY_ATTRIBUTES{
              nLength := BYTESIZE(WinBase.SECURITY_ATTRIBUTES),
              lpSecurityDescriptor := NIL, (* use caller's default *)
              bInheritHandle := 0};
  VAR fname := M3toC.SharedTtoS(p);
  BEGIN
    IF WinBase.CreateDirectory(fname, ADR(sa)) = False THEN
      Fail(p, fname);
    END;
    M3toC.FreeSharedS(p, fname);
  END CreateDirectory;

PROCEDURE DeleteDirectory(p: Pathname.T) RAISES {OSError.E}=
  VAR fname := M3toC.SharedTtoS(p);
  BEGIN
    IF WinBase.RemoveDirectory(fname) = False THEN
      Fail(p, fname);
    END;
    M3toC.FreeSharedS(p, fname);
  END DeleteDirectory;
  
PROCEDURE DeleteFile(p: Pathname.T) RAISES {OSError.E}=
  VAR fname := M3toC.SharedTtoS(p);
  BEGIN 
    IF WinBase.DeleteFile(fname) = False THEN
      Fail(p, fname);
    END;
    M3toC.FreeSharedS(p, fname);
  END DeleteFile;

PROCEDURE Rename(p0, p1: Pathname.T) RAISES {OSError.E} =
  VAR
    err: INTEGER;
    f0 := M3toC.SharedTtoS(p0);
    f1 := M3toC.SharedTtoS(p1);
  BEGIN
    IF WinBase.MoveFileEx(f0, f1, WinBase.MOVEFILE_REPLACE_EXISTING) # 0 THEN
      M3toC.FreeSharedS(p0, f0);
      M3toC.FreeSharedS(p1, f1);
      RETURN;
    END;

    err := WinBase.GetLastError();
    IF (err # WinError.ERROR_CALL_NOT_IMPLEMENTED) THEN
      M3toC.FreeSharedS(p0, f0);
      M3toC.FreeSharedS(p1, f1);
      OSErrorWin32.Raise0(err);
    END;

    (* MoveFileEx is not implemented on Win95.  What a bunch of crap! *)
    IF WinBase.MoveFile(f0, f1) # 0 THEN
      M3toC.FreeSharedS(p0, f0);
      M3toC.FreeSharedS(p1, f1);
      RETURN;
    END;

    err := WinBase.GetLastError();
    M3toC.FreeSharedS(p0, f0);
    M3toC.FreeSharedS(p1, f1);
    OSErrorWin32.Raise0(err);
  END Rename;

REVEAL Iterator = PublicIterator BRANDED OBJECT
    handle: WinNT.HANDLE;
    done := FALSE;
    first := TRUE;
    ffd: WinBase.WIN32_FIND_DATA
  OVERRIDES
    next := IterNext;
    nextWithStatus := IterNextWithStatus;
    close := IterClose
  END;

PROCEDURE Iterate(p: Pathname.T): Iterator RAISES {OSError.E} =
  VAR
    iter     := NEW(Iterator);
    allFiles := Pathname.Join (p, "*", NIL);
    pattern  := M3toC.SharedTtoS(allFiles);
    handle   := WinBase.FindFirstFile(pattern, ADR(iter.ffd));
  BEGIN
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN
      Fail(allFiles, pattern);
    END;
    M3toC.FreeSharedS(allFiles, pattern);
    iter.handle := handle;
    RETURN iter;
  END Iterate;

PROCEDURE IterNext(iter: Iterator; VAR (*out*) name: TEXT): BOOLEAN =
  VAR s: Ctypes.char_star;
  BEGIN
    IF IterRaw(iter, s) THEN name := M3toC.CopyStoT(s); RETURN TRUE END;
    RETURN FALSE
  END IterNext;

PROCEDURE IterNextWithStatus(
    iter: Iterator;
    VAR (*out*) name: TEXT;
    VAR (*out*) status: File.Status)
  : BOOLEAN =
  VAR s: Ctypes.char_star;
  BEGIN
    IF IterRaw(iter, s) THEN
      name := M3toC.CopyStoT(s);
      BuildStatus (iter.ffd, status);
      RETURN TRUE
    END;
    RETURN FALSE
  END IterNextWithStatus;

EXCEPTION IterClosed; <* FATAL IterClosed *>

PROCEDURE IterRaw(iter: Iterator; VAR (*out*) s: Ctypes.char_star): BOOLEAN =
  BEGIN
    LOOP (* to ignore "." and ".." *)
      IF iter.done THEN RAISE IterClosed END;
      IF iter.handle = NIL THEN RETURN FALSE END;
      IF iter.first THEN iter.first := FALSE;
      ELSE
        WITH rc = WinBase.FindNextFile(iter.handle, ADR(iter.ffd)) DO
          IF rc = 0 THEN
            WITH e = WinBase.GetLastError() DO
              IF e = WinError.ERROR_NO_MORE_FILES THEN
                EVAL WinBase.FindClose(iter.handle);
                iter.handle := NIL;
                RETURN FALSE
              ELSE
                <* FATAL OSError.E *> BEGIN OSErrorWin32.Raise() END
              END;
            END;
          END;
        END;
      END;
      s := ADR(iter.ffd.cFileName);
      IF NOT DotOrDotDot(LOOPHOLE(s, UNTRACED REF CHAR)) THEN
	RETURN TRUE
      END
      (* else continue to next entry *)
    END
  END IterRaw;

PROCEDURE IterClose(iter: Iterator) =
  BEGIN
    IF iter.handle # NIL THEN
      EVAL WinBase.FindClose(iter.handle); iter.handle := NIL
     END;
    iter.done := TRUE
  END IterClose;

PROCEDURE DotOrDotDot(n: UNTRACED REF CHAR): BOOLEAN =
  BEGIN
    IF n^ # '.' THEN RETURN FALSE END;
    INC(n);
    IF n^ = '\000' THEN RETURN TRUE;   (* "." *)
    ELSIF n^ # '.' THEN RETURN FALSE   (* ".x" *)
    END;
    INC(n);
    RETURN n^ = '\000'                 (* ".." or "..x" *)
  END DotOrDotDot;

PROCEDURE Status(p: Pathname.T): File.Status RAISES {OSError.E} =
  VAR
    ffd    : WinBase.WIN32_FIND_DATA;
    stat   : File.Status;
    fname  := M3toC.SharedTtoS(p);
    handle := WinBase.FindFirstFile(fname, ADR(ffd));
  BEGIN
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN
      (* if "FindFirstFile" didn't work, try getting its status only *)
      VAR
        err := WinBase.GetLastError(); (* first, remember previous error *)
        attrs := WinBase.GetFileAttributes(fname);
      BEGIN
        IF attrs # 16_FFFFFFFF AND (* indicates GetFileAttributes failure *)
           Word.And(attrs, WinNT.FILE_ATTRIBUTE_DIRECTORY) # 0 THEN
          (* "p" names a directory; we don't have to set the other fields *)
          stat.type := DirectoryFileType
        ELSE
          M3toC.FreeSharedS(p, fname);
          OSErrorWin32.Raise0(err);
        END;
      END;
    ELSE
      BuildStatus (ffd, (*OUT*) stat);
      EVAL WinBase.FindClose(handle);
    END;
    M3toC.FreeSharedS(p, fname);
    RETURN stat;
  END Status;

PROCEDURE BuildStatus (READONLY ffd  : WinBase.WIN32_FIND_DATA;
                     VAR(*OUT*) stat : File.Status) =
  BEGIN
    stat.size := Long.LeftShift(VAL(ffd.nFileSizeHigh, LONGINT), 32) + VAL(ffd.nFileSizeLow, LONGINT);
    stat.modificationTime := TimeWin32.FromFileTime(ffd.ftLastWriteTime);
    IF Word.And(ffd.dwFileAttributes, WinNT.FILE_ATTRIBUTE_DIRECTORY) # 0
      THEN stat.type := DirectoryFileType;
      ELSE stat.type := RegularFile.FileType; (* more or less... *)
    END;
  END BuildStatus;

PROCEDURE SetModificationTime(p: Pathname.T; READONLY t: Time.T)
  RAISES {OSError.E} =
  VAR h: File.T; lastWrite: WinBase.FILETIME;
  BEGIN
    TimeWin32.ToFileTime(t, lastWrite);
    h := OpenFileReadonly(p);
    TRY
      IF WinBase.SetFileTime(
           hFile := h.handle,
           lpCreationTime := NIL,
           lpLastAccessTime := NIL,
           lpLastWriteTime := ADR(lastWrite)) = 0 THEN OSErrorWin32.Raise()
      END
    FINALLY h.close()      
    END
  END SetModificationTime;

PROCEDURE Fail(p: Pathname.T;  f: Ctypes.char_star) RAISES {OSError.E} =
  VAR err := WinBase.GetLastError();
  BEGIN
    M3toC.FreeSharedS(p, f);
    OSErrorWin32.Raise0(err);
  END Fail;

BEGIN
END FSWin32.
