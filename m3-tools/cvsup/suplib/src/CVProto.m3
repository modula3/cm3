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

(* To add a new protocol version, define the new fields in the interface
   file.  Then, add code to the module initialization block to
   enter the new protocol entry into the table. *)

MODULE CVProto;

IMPORT
  Glob, IntRefTbl, Rd, SupMisc, Thread, TokScan, Word, Wr;

VAR
  tbl: IntRefTbl.T;

(*****************************************************************************)
(* The original object, which does not escape white space in the protocol. *)
(*****************************************************************************)

TYPE
  T0 = T OBJECT OVERRIDES
    getCmd := T0GetCmd;
    putCmd := T0PutCmd;
  END;

PROCEDURE T0GetCmd(<*UNUSED*> self: T0; rd: Rd.T): TokScan.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN TokScan.New(SupMisc.GetCmdLine(rd));
  END T0GetCmd;

PROCEDURE T0PutCmd(<*UNUSED*> self: T0;
                   wr: Wr.T;
                   cmd: TEXT;
                   f0, f1, f2, f3, f4, f5, f6, f7, f8, f9: TEXT := NIL;
		   more := FALSE)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    SupMisc.PutCmd(wr,
		   cmd,
		   f0, f1, f2, f3, f4, f5, f6, f7, f8, f9,
		   more,
		   encode := FALSE);
  END T0PutCmd;

(*****************************************************************************)
(* New improved version that can handle white space in file names and other
   places *)
(*****************************************************************************)

TYPE
  T1 = T0 OBJECT OVERRIDES
    getCmd := T1GetCmd;
    putCmd := T1PutCmd;
  END;

PROCEDURE T1GetCmd(<*UNUSED*> self: T1; rd: Rd.T): TokScan.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN TokScan.NewDec(SupMisc.GetCmdLine(rd));
  END T1GetCmd;

PROCEDURE T1PutCmd(<*UNUSED*> self: T1;
                   wr: Wr.T;
                   cmd: TEXT;
                   f0, f1, f2, f3, f4, f5, f6, f7, f8, f9: TEXT := NIL;
		   more := FALSE)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    SupMisc.PutCmd(wr,
		   cmd,
		   f0, f1, f2, f3, f4, f5, f6, f7, f8, f9,
		   more,
		   encode := TRUE);
  END T1PutCmd;

(*****************************************************************************)
(* Other procedures. *)
(*****************************************************************************)

PROCEDURE Encode(major, minor: VersionNumber): INTEGER =
  BEGIN
    RETURN Word.Or(Word.LeftShift(major, 16), minor);
  END Encode;

PROCEDURE Enter(proto: T) =
  VAR
    exists: BOOLEAN;
  BEGIN
    exists := tbl.put(Encode(proto.major, proto.minor), proto);
    <* ASSERT NOT exists *>
  END Enter;

PROCEDURE HasS1GBug(proto: T; versionString: TEXT): BOOLEAN =
  CONST
    GoodVersions = ARRAY [0..2] OF TEXT {
      "SNAP_16_1[d-z]",
      "REL_16_1p[2-9]",
      "REL_16_1p1[0-9]"
    };
  BEGIN
    (* If the protocol indicates that the bug is not present, we are OK. *)
    IF NOT proto.v.hasS1GBug THEN
      RETURN FALSE;
    END;

    (* Check for a few specific versions in which the bug was fixed but
       the protocol version was still 16.1. *)
    IF proto.major = 16 AND proto.minor = 1 THEN
      FOR i := FIRST(GoodVersions) TO LAST(GoodVersions) DO
	IF Glob.Match(GoodVersions[i], versionString) THEN
	  RETURN FALSE;
	END;
      END;
    END;

    (* All other versions have the bug. *)
    RETURN TRUE;
  END HasS1GBug;

PROCEDURE Lookup(major, minor: VersionNumber): T
  RAISES {NotSupported} =
  VAR
    ref: REFANY;
  BEGIN
    IF NOT tbl.get(Encode(major, minor), ref) THEN
      RAISE NotSupported;
    END;
    RETURN ref;
  END Lookup;

PROCEDURE Resolve(major, minor: VersionNumber): T
  RAISES {NotSupported} =
  BEGIN
    IF Current.major < major
    OR Current.major = major AND Current.minor < minor THEN
      major := Current.major;
      minor := Current.minor;
    END;
    RETURN Lookup(major, minor);
  END Resolve;

BEGIN
  tbl := NEW(IntRefTbl.Default).init();

  Current := NEW(T0, major := 14, minor := 2);
  Enter(Current);

  Current := NEW(T0, major := 15, minor := 0, v := Current.v);
  Current.v.hasFileAttrs := TRUE;
  Current.v.hasHardLinks := TRUE;
  Current.v.dirsAreExplicit := TRUE;
  Current.v.serverSendsFilter := FALSE;
  Current.v.hasClientAccepts := TRUE;
  Enter(Current);

  Current := NEW(T0, major := 15, minor := 1, v := Current.v);
  Current.v.hasKeywordControl := TRUE;
  Enter(Current);

  Current := NEW(T0, major := 15, minor := 2, v := Current.v);
  Current.v.hasMuxMode := TRUE;
  Current.v.exchangesVersions := TRUE;
  Enter(Current);

  Current := NEW(T0, major := 15, minor := 3, v := Current.v);
  Current.v.hidesAtticInCVSHeader := TRUE;
  Current.v.sendsRevDates := TRUE;
  Enter(Current);

  Current := NEW(T0, major := 15, minor := 4, v := Current.v);
  Current.v.clientSendsUmask := TRUE;
  Enter(Current);

  (* Here we switch to type T1, which encodes white space. *)

  Current := NEW(T1, major := 15, minor := 5, v := Current.v);
  Current.v.handlesWhiteSpace := TRUE;
  Current.v.hasLooseRCSCheck := TRUE;
  Enter(Current);

  Current := NEW(T1, major := 15, minor := 6, v := Current.v);
  Current.v.sendsDeltaPhrases := TRUE;
  Enter(Current);

  Current := NEW(T1, major := 16, minor := 0, v := Current.v);
  Current.v.hasMD5Auth := TRUE;
  Enter(Current);

  Current := NEW(T1, major := 16, minor := 1, v := Current.v);
  Current.v.hasRsyncFilter := TRUE;
  Enter(Current);

  Current := NEW(T1, major := 17, minor := 0, v := Current.v);
  Current.v.hasS1GBug := FALSE;
  Enter(Current);
END CVProto.
