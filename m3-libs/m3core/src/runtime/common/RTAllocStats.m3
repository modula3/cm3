(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(*| Last modified on Fri May  5 10:13:33 PDT 1995 by kalsow  *)
(*|      modified on Wed Mar  8 14:21:02 PST 1995 by detlefs *)

UNSAFE MODULE RTAllocStats;

IMPORT RT0, RT0u, RTMisc, RTType, Word, RTHeapRep, RTAllocator;
IMPORT RTStack, RTProcedure, RTProcedureSRC, Text, Convert;
FROM RT0 IMPORT Typecode;

CONST
  MaxSiteID = 255; (* == LAST (RT0.RefHeader.spare) *)
  MaxDepth  = 6;

TYPE
  Site = RECORD
    hash : Word.T;
    pcs  : ARRAY [0..MaxDepth] OF ADDRESS;
  END;

  SiteList = BRANDED REF ARRAY OF Site;

  TypeInfo = REF RECORD
    n_sites : INTEGER  := 0;
    sites   : SiteList := NIL;
  END;

  InfoList = BRANDED REF ARRAY OF TypeInfo;

VAR
  info : InfoList := NIL;

PROCEDURE EnableTrace (tc: Typecode) =
  BEGIN
    IF NOT RTStack.Has_walker THEN RETURN END;
    IF NOT RTType.IsTraced (tc) THEN
      RTMisc.FatalError(NIL, 0,
                        "RTAllocStats.EnableTrace(untraced type) was called");
    END;
    IF (info = NIL) THEN
      info := NEW (InfoList, RT0u.nTypes);
      RTAllocator.callback := NoteAllocation;
    END;
    info [tc] := NEW (TypeInfo, sites := NEW (SiteList, 4));
  END EnableTrace;

PROCEDURE NSites (tc: Typecode): INTEGER =
  VAR inf: TypeInfo := NIL;
  BEGIN
    IF (info = NIL) THEN RETURN -1; END;
    inf := info[tc];
    IF (inf = NIL) THEN RETURN -1; END;
    RETURN inf.n_sites;
  END NSites;

PROCEDURE GetSiteText (tc: Typecode;  site, depth: CARDINAL): TEXT =
  VAR inf: TypeInfo := NIL;
  BEGIN
    IF (info = NIL) THEN RETURN NIL; END;
    inf := info[tc];

    IF (inf = NIL) THEN
      RETURN NIL;

    ELSIF (site = 0) THEN
      IF (depth > 0)
        THEN RETURN NIL;
        ELSE RETURN "OTHER SITES";
      END;

    ELSIF (site > inf.n_sites) OR (depth > MaxDepth) THEN
      RETURN NIL;

    ELSE
      RETURN PcToText (inf.sites[site-1].pcs[depth]);

    END;
  END GetSiteText;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE NoteAllocation (ref: REFANY) =
  VAR inf: TypeInfo;  s: INTEGER;
  BEGIN
    IF (info = NIL) THEN RETURN END;
    inf := info [TYPECODE (ref)];
    IF (inf = NIL) THEN RETURN END;

    IF (inf.n_sites > LAST (inf.sites^)) THEN ExpandSites (inf) END;

    WITH z = inf.sites [inf.n_sites] DO
      GetSite (z, 2);
      s := 0; WHILE (inf.sites[s].hash # z.hash) DO INC (s); END;
      IF (s < MaxSiteID) THEN
        IF (s >= inf.n_sites) THEN (* new site! *) INC (inf.n_sites); END;
        InsertSiteNum (ref, s+1);
      END;
    END;
  END NoteAllocation;

PROCEDURE ExpandSites (inf: TypeInfo) =
  VAR n := NUMBER (inf.sites^);  new := NEW (SiteList, MIN (n+n, MaxSiteID+1));
  BEGIN
    SUBARRAY (new^, 0, n) := inf.sites^;
    inf.sites := new;
  END ExpandSites;

PROCEDURE InsertSiteNum (ref: REFANY;  sitetag: INTEGER) =
  VAR
    addr := LOOPHOLE (ref, ADDRESS);
    hdr  := LOOPHOLE (addr - BYTESIZE(RT0.RefHeader), RTHeapRep.RefHeader);
  BEGIN
    hdr^.spare := sitetag;
  END InsertSiteNum;

PROCEDURE GetSite (VAR(*OUT*) site: Site;  skip: CARDINAL) =
  VAR cur, prev: RTStack.Frame;
  BEGIN
    RTStack.CurrentFrame (cur);

    FOR i := 0 TO skip-1 DO
      RTStack.PreviousFrame (cur, prev);
      cur := prev;
    END;

    site.hash := 0;
    FOR i := 0 TO LAST (site.pcs) DO
      IF (cur.pc # NIL) AND (i < siteDepth) THEN
        RTStack.PreviousFrame (cur, prev);  cur := prev;
        site.hash   := Word.Xor (site.hash, LOOPHOLE(cur.pc, Word.T));
        site.pcs[i] := cur.pc;
      ELSE
        site.pcs[i] := NIL;
      END;
    END;
  END GetSite;

PROCEDURE PcToText (pc: ADDRESS): TEXT =
  <*FATAL Convert.Failed*>
  CONST NUL = '\000';
  VAR
    proc : RTProcedure.Proc;
    file : RTProcedureSRC.Name;
    name : RTProcedureSRC.Name;
    cp   : RT0.String;
    cur  : INTEGER := 0;
    len  : INTEGER;
    buf  : ARRAY [0..511] OF CHAR;
  BEGIN
    RTProcedureSRC.FromPC (pc, proc, file, name);
    IF (proc = NIL) THEN RETURN NIL END;

    cp := name;
    WHILE (cp # NIL) AND (cp^ # NUL) DO
      buf[cur] := cp^;  INC (cur);  INC (cp, ADRSIZE(cp^));
    END;

    IF (name # NIL) AND (pc # proc) THEN
      buf[cur] := ' ';  INC (cur);
      buf[cur] := '+';  INC (cur);
      buf[cur] := ' ';  INC (cur);
      len := Convert.FromUnsigned (
               SUBARRAY (buf, cur, NUMBER (buf) - cur),
               pc - proc, base := 16, prefix := TRUE);
      INC (cur, len);
    END;

    (* remove any path components from the file name *)
    cp := file;
    WHILE (cp # NIL) AND (cp^ # NUL) DO
      IF (cp^ = '/') OR (cp^ = '\134') THEN
        file := cp + ADRSIZE (cp^);
      END;
      INC (cp, ADRSIZE (cp^));
    END;

    IF (file # NIL) THEN
      buf[cur] := ' ';  INC (cur);
      buf[cur] := 'i';  INC (cur);
      buf[cur] := 'n';  INC (cur);
      buf[cur] := ' ';  INC (cur);
    END;

    cp := file;
    WHILE (cp # NIL) AND (cp^ # NUL) DO
      buf[cur] := cp^;  INC (cur);  INC (cp, ADRSIZE(cp^));
    END;

    RETURN Text.FromChars (SUBARRAY (buf, 0, cur));
  END PcToText;

BEGIN
END RTAllocStats.

