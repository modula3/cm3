(* Copyright (C) 1993, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Portions Copyright 1996, Critical Mass, Inc.             *)
(*                                                          *)
(* Last modified on Thu May  2 13:35:47 PDT 1996 by heydon  *)
(*      modified on Thu Aug 31 14:02:00 PDT 1995 by steveg  *)
(*      modified on Wed Nov 30 13:44:49 PST 1994 by kalsow  *)
(*      modified on Fri Feb 18 10:45:30 PST 1994 by mcjones *)
(*      modified on Fri May  7 22:56:07 PDT 1993 by mjordan *)

UNSAFE MODULE FSWin32 EXPORTS FS;

IMPORT Ctypes, File, FileWin32, M3toC, OSError, OSErrorWin32, OSWin32,
  Pathname, RegularFile, Text, Time, TimeWin32, WinBase, WinDef,
  WinError, WinNT, Word;

CONST
  False: WinDef.BOOL = 0;
  True: WinDef.BOOL = 1;

EXCEPTION InternalError; <* FATAL InternalError *>

PROCEDURE GetAbsolutePathname(p: Pathname.T): Pathname.T
  RAISES {OSError.E} =
  VAR lpFileName := M3toC.TtoS(p);
  PROCEDURE DoIt(VAR chars: ARRAY OF CHAR): WinDef.DWORD
    RAISES {OSError.E} =
    VAR filePart: WinNT.LPSTR;
    BEGIN
      WITH n = WinBase.GetFullPathName(
                 lpFileName := lpFileName,
                 nBufferLength := NUMBER(chars),
                 lpBuffer := ADR(chars[0]),
                 lpFilePart := ADR(filePart)) DO
        IF n = 0 THEN OSErrorWin32.Raise() END;
        RETURN n
      END
    END DoIt;
  VAR chars: ARRAY [0..63] OF CHAR; n := DoIt(chars);
  BEGIN
    IF n < NUMBER(chars) THEN
      RETURN Text.FromChars(SUBARRAY(chars, 0, n))
    END;
    WITH refChars = NEW(REF ARRAY OF CHAR, n + 1) DO
      n := DoIt(refChars^);
      IF n > NUMBER(refChars^) THEN RAISE InternalError END;
      RETURN Text.FromChars(SUBARRAY(refChars^, 0, n))
    END
  END GetAbsolutePathname;

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
    handle := WinBase.CreateFile(
      lpFileName := M3toC.TtoS(p),
      dwDesiredAccess := WinNT.GENERIC_READ + WinNT.GENERIC_WRITE,
      dwShareMode := WinNT.FILE_SHARE_READ + WinNT.FILE_SHARE_WRITE,
      lpSecurityAttributes := lpsa,
      dwCreationDisposition := createMode[create, truncate],
      dwFlagsAndAttributes := attrs,
      hTemplateFile := handleTemplate);
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN 
      OSErrorWin32.Raise()
    END;
    RETURN FileWin32.New(handle, FileWin32.ReadWrite)
  END OpenFile;

PROCEDURE GetFileAttributes(handle: WinNT.HANDLE): WinDef.DWORD
  RAISES {OSError.E} =
  VAR info: WinBase.BY_HANDLE_FILE_INFORMATION;
  BEGIN
    IF WinBase.GetFileInformationByHandle(handle, ADR(info)) = False THEN
      OSErrorWin32.Raise()
    END;
    RETURN info.dwFileAttributes;
  END GetFileAttributes;

PROCEDURE GetFileSecurityDescriptor(pn: Pathname.T): REF ARRAY OF WinDef.BYTE
  RAISES {OSError.E} =
  VAR rsd: REF ARRAY OF WinDef.BYTE; n, nNeeded: WinDef.DWORD;
  CONST Info = WinNT.OWNER_SECURITY_INFORMATION +
               WinNT.GROUP_SECURITY_INFORMATION +
               WinNT.DACL_SECURITY_INFORMATION +
               WinNT.SACL_SECURITY_INFORMATION;
  BEGIN
    IF OSWin32.Win95() THEN RETURN NIL END; 
    (* WinBase.GetFileSecurity not implement in Win95 *)

    n := 64;
    LOOP
      rsd := NEW(REF ARRAY OF WinDef.BYTE, n);
      IF WinBase.GetFileSecurity(
           lpFileName := M3toC.TtoS(pn),
           RequestedInformation := Info,
           pSecurityDescriptor := ADR(rsd[0]),
           nLength := n,
           lpnLengthNeeded := ADR(nNeeded)) = False THEN OSErrorWin32.Raise()
      END;
      IF nNeeded = 0 THEN EXIT END;
      n := nNeeded
    END;
    RETURN rsd
  END GetFileSecurityDescriptor;

PROCEDURE OpenFileReadonly(p: Pathname.T): File.T RAISES {OSError.E}=
  VAR handle: WinNT.HANDLE;
  BEGIN
    handle := WinBase.CreateFile(
      lpFileName := M3toC.TtoS(p),
      dwDesiredAccess := WinNT.GENERIC_READ,
      dwShareMode :=  WinNT.FILE_SHARE_READ,
      lpSecurityAttributes := NIL,
      dwCreationDisposition := WinBase.OPEN_EXISTING,
      dwFlagsAndAttributes := 0,
      hTemplateFile := NIL);
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN 
      OSErrorWin32.Raise()
    END;
    RETURN FileWin32.New(handle, FileWin32.Read)
  END OpenFileReadonly;

PROCEDURE CreateDirectory(p: Pathname.T) RAISES {OSError.E}=
  VAR sa := WinBase.SECURITY_ATTRIBUTES{
              nLength := BYTESIZE(WinBase.SECURITY_ATTRIBUTES),
              lpSecurityDescriptor := NIL, (* use caller's default *)
              bInheritHandle := 0};
  BEGIN 
    IF WinBase.CreateDirectory(M3toC.TtoS(p), ADR(sa)) = False THEN
      OSErrorWin32.Raise()
    END
  END CreateDirectory;

PROCEDURE DeleteDirectory(p: Pathname.T) RAISES {OSError.E}=
  BEGIN 
    IF WinBase.RemoveDirectory(M3toC.TtoS(p)) = False THEN
      OSErrorWin32.Raise()
    END
  END DeleteDirectory;

PROCEDURE DeleteFile(p: Pathname.T) RAISES {OSError.E}=
  BEGIN 
    IF WinBase.DeleteFile(M3toC.TtoS(p)) = False THEN
      OSErrorWin32.Raise()
    END
  END DeleteFile;

PROCEDURE Rename(p0, p1: Pathname.T) RAISES {OSError.E} =
  VAR err: INTEGER;
  BEGIN 
    IF WinBase.MoveFileEx(M3toC.TtoS(p0), M3toC.TtoS(p1),
                          WinBase.MOVEFILE_REPLACE_EXISTING) = 0 THEN
      err := WinBase.GetLastError();
      IF (err = WinError.ERROR_CALL_NOT_IMPLEMENTED) THEN
        (* MoveFileEx is not implemented on Win95 *)
        IF WinBase.MoveFile(M3toC.TtoS(p0), M3toC.TtoS(p1)) = 0 THEN
          OSErrorWin32.Raise();
        END;
        RETURN;
      END;
      OSErrorWin32.Raise0(err)
    END
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
    allFiles := Pathname.Join(p, "*", NIL);
    handle   := WinBase.FindFirstFile(M3toC.TtoS(allFiles), ADR(iter.ffd));
  BEGIN
    IF LOOPHOLE(handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN
      OSErrorWin32.Raise()
    END;
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
    fname  := M3toC.TtoS(p);
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
          OSErrorWin32.Raise0(err)
        END
      END
    ELSE
      BuildStatus (ffd, (*OUT*) stat);
      EVAL WinBase.FindClose(handle);
    END;
    RETURN stat;
  END Status;

PROCEDURE BuildStatus (READONLY ffd  : WinBase.WIN32_FIND_DATA;
                     VAR(*OUT*) stat : File.Status) =
  BEGIN
    stat.size := ffd.nFileSizeLow;
    stat.modificationTime := TimeWin32.FromFileTime(ffd.ftLastWriteTime);
    IF Word.And(ffd.dwFileAttributes, WinNT.FILE_ATTRIBUTE_DIRECTORY) # 0
      THEN stat.type := DirectoryFileType;
      ELSE stat.type := RegularFile.FileType; (* more or less... *)
    END;
  END BuildStatus;

PROCEDURE SetModificationTime(p: Pathname.T; READONLY t: Time.T)
  RAISES {OSError.E} =
  VAR h: File.T; lastWrite := TimeWin32.ToFileTime(t);
  BEGIN
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

BEGIN
END FSWin32.
