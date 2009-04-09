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
 * $Id: SupFile.m3,v 1.1.1.1 2009-04-09 17:01:40 jkrell Exp $ *)

MODULE SupFile;

IMPORT
  ErrMsg, FileRd, OSError, Pathname, RCSDate, Rd, SupFileRec,
  SupFileRecSeq, SupMisc, Text, Thread, TokScan, UnixMisc, Uugid;

CONST
  PreserveOptions = SupFileRec.Options{  (* Options set for "preserve". *)
    SupFileRec.Option.SetOwner,
    SupFileRec.Option.SetFlags };

PROCEDURE Parse(fileName: Pathname.T;
		override: SupFileRec.T := NIL;
		mask := SupFileRec.Options{}): SupFileRecSeq.T
  RAISES {Error} =
  <* FATAL Thread.Alerted *>
  VAR
    rd: Rd.T;
    collections: SupFileRecSeq.T;
    line, field: TEXT;
    ts, ts2: TokScan.T;
    coll: TEXT;
    default: SupFileRec.T := NIL;
    sfr: SupFileRec.T;
    name, value: TEXT;
    noCheckRCS: BOOLEAN;
    dfltNoCheckRCS := FALSE;
  BEGIN
    TRY
      rd := FileRd.Open(fileName);
    EXCEPT OSError.E(list) =>
      RAISE Error("Cannot open \"" & fileName & "\": " &
	ErrMsg.StrError(list));
    END;
    TRY
      collections := NEW(SupFileRecSeq.T).init();
      LOOP
	TRY
	  line := Rd.GetLine(rd);
	EXCEPT
	| Rd.EndOfFile =>
	  EXIT;
	| Rd.Failure(list) =>
	  RAISE Error("Read failure from \"" & fileName & "\": " &
	    ErrMsg.StrError(list));
	END;
	WITH pos = Text.FindChar(line, '#') DO
	  IF pos >= 0 THEN line := Text.Sub(line, 0, pos) END;
	END;
	ts := TokScan.New(line);
	TRY
	  IF ts.next(coll) THEN
	    sfr := NEW(SupFileRec.T).init(default);
	    sfr.collection := coll;
	    noCheckRCS := dfltNoCheckRCS;

	    WHILE ts.next(field) DO
	      IF Text.FindChar(field, '=') >= 0 THEN
		ts2 := TokScan.New(field, SET OF CHAR{'='});
		name := ts2.getToken("field name");
		value := ts2.getToken("Field value");
		IF TokScan.EqualFolded(name, "release") THEN
		  IF NOT Pathname.Valid(value) THEN
		    RAISE Error("Invalid release \"" & value & "\"");
		  END;
		  sfr.release := value;
		ELSIF TokScan.EqualFolded(name, "host") THEN
		  sfr.serverHost := value;
		ELSIF TokScan.EqualFolded(name, "base") THEN
		  IF NOT Pathname.Valid(value) THEN
		    RAISE Error("Invalid base \"" & value & "\"");
		  END;
		  sfr.clientBase := value;
		ELSIF TokScan.EqualFolded(name, "prefix") THEN
		  IF NOT Pathname.Valid(value) THEN
		    RAISE Error("Invalid prefix \"" & value & "\"");
		  END;
		  sfr.clientPrefix := value;
		ELSIF TokScan.EqualFolded(name, "tag") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.CheckoutMode};
		  sfr.checkoutTag := value;
		ELSIF TokScan.EqualFolded(name, "date") THEN
		  IF NOT Text.Equal(value, ".")
		  AND NOT RCSDate.Valid(value) THEN
		    RAISE Error("Invalid date \"" & value & "\"");
		  END;
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.CheckoutMode};
		  sfr.checkoutDate := value;
		ELSIF TokScan.EqualFolded(name, "list") THEN
		  IF Text.FindChar(value, SupMisc.SlashChar) >= 0 THEN
		    RAISE Error("\"list\" suffix must not contain slashes");
		  END;
		  sfr.listSuffix := value;
		ELSIF TokScan.EqualFolded(name, "umask") THEN
		  sfr.umask := TokScan.AtoI(value, "umask value", 8);
		END;
	      ELSE
		IF TokScan.EqualFolded(field, "backup") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.Backup};
		ELSIF TokScan.EqualFolded(field, "delete") THEN
		  sfr.options := sfr.options + SupFileRec.Options{
		    SupFileRec.Option.Delete,
		    SupFileRec.Option.ExactRCS};
		ELSIF TokScan.EqualFolded(field, "keep") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.Keep};
		ELSIF TokScan.EqualFolded(field, "old") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.Old};
		ELSIF TokScan.EqualFolded(field, "unlinkbusy") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.UnlinkBusy};
		ELSIF TokScan.EqualFolded(field, "noupdate") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.NoUpdate};
		ELSIF TokScan.EqualFolded(field, "compress") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.Compress};
		ELSIF TokScan.EqualFolded(field, "use-rel-suffix") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.UseRelSuffix};
		ELSIF TokScan.EqualFolded(field, "norsync") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.NoRsync};
		ELSIF TokScan.EqualFolded(field, "norcs") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.NoRCS};
		ELSIF TokScan.EqualFolded(field, "nocheckrcs") THEN
		  noCheckRCS := TRUE;
		ELSIF TokScan.EqualFolded(field, "strictrcs") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.StrictCheckRCS};
		ELSIF TokScan.EqualFolded(field, "execute") THEN
		  sfr.options := sfr.options +
		    SupFileRec.Options{SupFileRec.Option.Execute};
		ELSIF TokScan.EqualFolded(field, "preserve") THEN
		  sfr.options := sfr.options + PreserveOptions;
		END;
	      END;
	    END;

	    IF Text.Equal(sfr.collection, "*default") THEN
	      default := sfr;
	      dfltNoCheckRCS := noCheckRCS;
	    ELSE
	      IF override # NIL THEN
		sfr.overrideFrom(override, mask);
	      END;

	      (* Determine whether to checksum RCS files or not. *)
	      IF SupFileRec.Option.ExactRCS IN sfr.options
	      AND NOT noCheckRCS THEN
		sfr.options := sfr.options +
		  SupFileRec.Options{SupFileRec.Option.CheckRCS};
	      ELSE
		sfr.options := sfr.options -
		  SupFileRec.Options{SupFileRec.Option.CheckRCS};
	      END;

	      (* In recent versions, we always try to set the file modes. *)
	      sfr.options := sfr.options + SupFileRec.Options{
		SupFileRec.Option.SetMode};
	      (* Ignore "preserve" in checkout mode. *)
	      IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
		sfr.options := sfr.options - PreserveOptions;
	      END;
	      (* Preserve all mode bits if "preserve" is set. *)
	      IF sfr.options >= PreserveOptions THEN
		sfr.umask := 0;
	      END;
	      (* If no umask was specified, default it to the OS umask. *)
	      IF sfr.umask = -1 THEN
		sfr.umask := UnixMisc.GetUmask();
	      END;
	      (* If the client is not the superuser, don't try to preserve
		 the owner, group, and flags.  FIXME - Unix specific. *)
	      IF Uugid.geteuid() # 0 THEN  (* FIXME - Unix specific. *)
		sfr.options := sfr.options - PreserveOptions;
	      END;

	      IF NOT Pathname.Valid(sfr.collection) THEN
		RAISE Error("Invalid collection \"" & sfr.collection & "\"");
	      END;
	      IF sfr.release = NIL THEN
		RAISE Error("Release not specified for collection \"" &
		  sfr.collection & "\"");
	      END;
	      IF sfr.serverHost = NIL THEN
		RAISE Error("Host not specified for collection \"" &
		  sfr.collection & "\"");
	      END;
	      IF collections.size() > 0
	      AND NOT TokScan.EqualFolded(sfr.serverHost,
		collections.gethi().serverHost)
	      THEN
		RAISE
		  Error("All \"host\" fields in the supfile must be the same");
	      END;

	      IF sfr.clientBase = NIL THEN
		sfr.clientBase := SupMisc.DefaultClientBase;
	      END;
	      IF NOT SupMisc.IsDirectory(sfr.clientBase) THEN
		RAISE Error("Nonexistent base directory \""
		  & sfr.clientBase & "\" for collection \""
		  & sfr.collection & "\"");
	      END;

	      IF sfr.clientPrefix = NIL THEN
		sfr.clientPrefix := sfr.clientBase;
	      ELSE
		sfr.clientPrefix :=
		  SupMisc.ResolvePath(sfr.clientBase, sfr.clientPrefix);
	      END;

	      IF sfr.clientCollDir = NIL THEN
		sfr.clientCollDir := SupMisc.DefaultClientCollDir;
	      END;

	      ParseRefuseFiles(sfr);
	      collections.addhi(sfr);
	    END;
	  END;
	EXCEPT TokScan.Error(msg) =>
	  RAISE Error("Parse error in \"" & fileName & "\": " & msg);
	END;
      END;
      IF collections.size() = 0 THEN
	RAISE Error("Empty supfile");
      END;
    FINALLY
      TRY
	Rd.Close(rd);
      EXCEPT Rd.Failure(list) =>
	RAISE Error("Cannot close \"" & fileName & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
    RETURN collections;
  END Parse;

PROCEDURE ParseRefuseFile(sfr: SupFileRec.T; filename: Pathname.T)
  RAISES {Error} =
(* Parses a "refuse" file, and records the relevant information
   in "sfr.refusals".  If the file does not exist, it is silently
   ignored. *)
  <* FATAL Thread.Alerted *>
  VAR
    rd: Rd.T;
    line: TEXT;
    ts: TokScan.T;
    tok: TEXT;
  BEGIN
    TRY
      rd := FileRd.Open(filename);
    EXCEPT OSError.E =>
      RETURN;
    END;
    TRY
      LOOP
	TRY
	  line := Rd.GetLine(rd);
	EXCEPT
	| Rd.EndOfFile =>
	  EXIT;
	| Rd.Failure(list) =>
	  RAISE Error("Read failure from \"" & filename & "\": " &
	    ErrMsg.StrError(list));
	END;
	ts := TokScan.New(line);
	<* FATAL TokScan.Error *>
	BEGIN
	  WHILE ts.next(tok) DO
	    sfr.refusals.addhi(tok);
	  END;
	END;
      END;
    FINALLY
      TRY
	Rd.Close(rd);
      EXCEPT Rd.Failure(list) =>
	RAISE Error("Cannot close \"" & filename & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END ParseRefuseFile;

PROCEDURE ParseRefuseFiles(sfr: SupFileRec.T)
  RAISES {Error} =
  VAR
    supDir := SupMisc.ResolvePath(sfr.clientBase, sfr.clientCollDir);
    collStem := SupMisc.CatPath(supDir,
      SupMisc.CatPath(sfr.collection, "refuse"));
  BEGIN
    (* First the global refuse file that applies to all collections. *)
    ParseRefuseFile(sfr, SupMisc.CatPath(supDir, "refuse"));

    (* Next the per-collection refuse file that applies to all release/tag
       combinations. *)
    ParseRefuseFile(sfr, collStem);

    (* Finally, the per-release and per-tag refuse file. *)
    WITH suffix = SupMisc.StatusFileSuffix(sfr) DO
      IF NOT Text.Empty(suffix) THEN  (* Different from per-collection. *)
	ParseRefuseFile(sfr, collStem & suffix);
      END;
    END;
  END ParseRefuseFiles;

BEGIN
END SupFile.
