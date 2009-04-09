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

UNSAFE MODULE UnixMisc;

IMPORT
  CText, Ctypes, File, FilePosix, IP, M3toC, OSError, OSErrorPosix,
  Pathname, Uerror, Umman, Unetdb, Unix, Usocket, Ustat, Utypes,
  Uutmp, Word;

VAR
  TheUmask: Utypes.mode_t;  (* Set once at module initialization time. *)

PROCEDURE AppendAlways(file: File.T)
  RAISES {OSError.E} =
  BEGIN
    WITH flags = Unix.fcntl(file.fd, Unix.F_GETFL, 0) DO
      IF flags = -1 THEN OSErrorPosix.Raise() END;
      IF Unix.fcntl(file.fd, Unix.F_SETFL, Word.Or(flags, Unix.O_APPEND)) = -1
      THEN
	OSErrorPosix.Raise();
      END;
    END;
  END AppendAlways;

PROCEDURE FStat(file: File.T;
                VAR statbuf: Ustat.struct_stat)
  RAISES {OSError.E} =
  BEGIN
    IF Ustat.fstat(file.fd, ADR(statbuf)) = -1 THEN
      OSErrorPosix.Raise();
    END; 
  END FStat;

PROCEDURE GetHostAddrs(host: TEXT): REF ARRAY OF IP.Address =
  VAR
    hostStr: Ctypes.char_star;
    h: Unetdb.struct_hostent_star;
    app: Ctypes.char_star_star;
    addrs: REF ARRAY OF IP.Address;
    numAddrs := 0;
  BEGIN
    hostStr := CText.SharedTtoS(host);
    h := Unetdb.gethostbyname(hostStr);
    CText.FreeSharedS(host, hostStr);
    IF h = NIL OR h.h_addrtype # Usocket.AF_INET OR h.h_addr_list = NIL THEN
      RETURN NIL;
    END;
    app := h.h_addr_list;
    WHILE app^ # NIL DO
      INC(numAddrs);
      INC(app, ADRSIZE(app^));
    END;
    addrs := NEW(REF ARRAY OF IP.Address, numAddrs);
    app := h.h_addr_list;
    FOR i := 0 TO numAddrs-1 DO
      addrs[i] := LOOPHOLE(app^, UNTRACED REF IP.Address)^;
      INC(app, ADRSIZE(app^));
    END;
    RETURN addrs;
  END GetHostAddrs;

PROCEDURE GetHostName(): TEXT
  RAISES {OSError.E} =
  VAR
    name: ARRAY [0..255] OF CHAR;
  BEGIN
    WITH sp = LOOPHOLE(ADR(name[0]), Ctypes.char_star) DO
      IF Unix.gethostname(sp, BYTESIZE(name)) = -1 THEN
	OSErrorPosix.Raise();
      END;
      RETURN M3toC.CopyStoT(sp);
    END;
  END GetHostName;

PROCEDURE GetLogin(): TEXT =
  VAR
    logName: Ctypes.char_star;
  BEGIN
    logName := Uutmp.getlogin();
    IF logName = NIL THEN
      RETURN NIL;
    END;
    RETURN M3toC.CopyStoT(logName);
  END GetLogin;

PROCEDURE GetMode(path: Pathname.T): Utypes.mode_t
  RAISES {OSError.E} =
  VAR
    statbuf: Ustat.struct_stat;
  BEGIN
    Stat(path, statbuf);
    RETURN statbuf.st_mode;
  END GetMode;

PROCEDURE GetUmask(): Utypes.mode_t =
  BEGIN
    RETURN TheUmask;
  END GetUmask;

PROCEDURE MapFile(p: Pathname.T;
                  VAR statbuf: Ustat.struct_stat): ADDRESS
  RAISES {OSError.E} =
  VAR
    pStr: Ctypes.char_star;
    addr: ADDRESS;
    fd: Ctypes.int;
  BEGIN
    pStr := CText.SharedTtoS(p);
    fd := Unix.open(pStr, Unix.O_RDONLY, 0);
    CText.FreeSharedS(p, pStr);
    IF fd = -1 THEN OSErrorPosix.Raise() END;
    TRY
      IF Ustat.fstat(fd, ADR(statbuf)) = -1 THEN
	OSErrorPosix.Raise();
      END; 
      IF Word.And(statbuf.st_mode, Ustat.S_IFMT) # Ustat.S_IFREG THEN
	OSErrorPosix.Raise0(Uerror.EINVAL);
      END;
      IF statbuf.st_size # 0 THEN
	addr := Umman.mmap(NIL, statbuf.st_size, Umman.PROT_READ,
	  Umman.MAP_SHARED, fd, 0);
	IF addr = LOOPHOLE(-1, ADDRESS) THEN OSErrorPosix.Raise() END;
      ELSE
	addr := NIL;
      END;
      RETURN addr;
    FINALLY
      EVAL Unix.close(fd);
    END;
  END MapFile;

PROCEDURE MaskMode(mode: Utypes.mode_t; umask := -1): Utypes.mode_t =
  BEGIN
    IF umask = -1 THEN umask := TheUmask END;
    RETURN Word.And(mode, Word.Not(umask));
  END MaskMode;

PROCEDURE ReadLink(path: Pathname.T): TEXT
  RAISES {OSError.E} =
  VAR
    pathStr: Ctypes.char_star;
    buff: ARRAY [0..1023] OF CHAR;
    len: INTEGER;
  BEGIN
    WITH sp = LOOPHOLE(ADR(buff[0]), Ctypes.char_star) DO
      pathStr := CText.SharedTtoS(path);
      len := Unix.readlink(pathStr, sp, BYTESIZE(buff));
      CText.FreeSharedS(path, pathStr);
      IF len = -1 THEN
	OSErrorPosix.Raise();
      END;
      IF len > LAST(buff) THEN
	OSErrorPosix.Raise0(Uerror.ENAMETOOLONG);
      END;
      buff[len] := '\000';
      RETURN M3toC.CopyStoT(sp);
    END;
  END ReadLink;

PROCEDURE Stat(path: Pathname.T;
               VAR statbuf: Ustat.struct_stat)
  RAISES {OSError.E} =
  VAR
    pathStr: Ctypes.char_star;
    r: Ctypes.int;
  BEGIN
    pathStr := CText.SharedTtoS(path);
    r := Ustat.stat(pathStr, ADR(statbuf));
    CText.FreeSharedS(path, pathStr);
    IF r = -1 THEN
      OSErrorPosix.Raise();
    END; 
  END Stat;

PROCEDURE Unmap(adr: ADDRESS; size: Word.T) RAISES {OSError.E} =
  BEGIN
    IF Umman.munmap(adr, size) = -1 THEN OSErrorPosix.Raise() END;
  END Unmap;

BEGIN
  TheUmask := Unix.umask(8_777);
  EVAL Unix.umask(TheUmask);
END UnixMisc.
