(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:26:45 PST 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:22:58 PDT 1993 by muller     *)

(* The linker generates an inital direct call to this module's
   main body.  All Modula-3 code reached from here. *)

UNSAFE MODULE RTMain;

IMPORT RT0, RT0u, RTParams, RTHeapRep, RTMisc;
IMPORT RTTypeSRC, RTSignal, RTHooks, RTThreadInit, RTHeapInfo;

FROM RTLinker IMPORT info, LinkInfo;

IMPORT (* libm3 *)
  ASCII, Atom, AtomAtomTbl, AtomIntTbl, AtomList, AtomListSort, AtomPQ, 
  AtomPQRep, AtomRefTbl, AtomSeq, AtomSeqRep, AtomTextTbl, AutoFlushWr, Axis, 
  Boolean, Bundle, BundleRep, Capability, Char, ConvertPacking, ETimer, Env, FS, 
  File, FileRd, FileWr, Fmt, FmtBuf, FmtBufF, FmtBufTest, Formatter, IO, Int32, 
  IntArraySort, IntAtomTbl, IntIntTbl, IntList, IntListSort, IntPQ, IntPQRep, 
  IntRefTbl, IntSeq, IntSeqRep, IntTextTbl, Integer, Interval, Lex, 
  LongFloatExtras, LongSqrt, LongrealType, LowPerfTool, MxConfig, MProperty, 
  MPropertyF, MachineID, Math, MsgRd, MsgWr, NullRd, NullWr, OSError, PackingTbl, 
  PackingTypeCode, Params, Path, PathPrivate, Pathname, PerfTool, Pickle, 
  PickleRd, PickleStubs, Pipe, PklAction, PklActionSeq, PklActionSeqRep, 
  PklTipeMap, Point, PolyRegion, Process, Property, PropertyF, PropertyV, Random, 
  RandomPerm, RandomReal, Rd, RdCopy, RdUtils, RealFloatExtras, RealSqrt, 
  RealType, Rect, RefAtomTbl, RefIntTbl, RefList, RefListSort, RefPQ, RefPQRep, 
  RefRefTbl, RefSeq, RefSeqRep, RefTextTbl, Refany, Region, RegionRep, 
  RegularFile, Scan, SortedAtomAtomTbl, SortedAtomIntTbl, SortedAtomRefTbl, 
  SortedAtomTextTbl, SortedIntAtomTbl, SortedIntIntTbl, SortedIntRefTbl, 
  SortedIntTextTbl, SortedRefAtomTbl, SortedRefIntTbl, SortedRefRefTbl, 
  SortedRefTextTbl, SortedTextAtomTbl, SortedTextIntTbl, SortedTextRefTbl, 
  SortedTextTextTbl, Stat, Stdio, Swap, Sx, Terminal, TextArraySort, TextAtomTbl, 
  TextIntTbl, TextList, TextListSort, TextPQ, TextPQRep, TextRd, TextRefTbl, 
  TextSeq, TextSeqRep, TextTextTbl, TextWr, TimeStamp, TimeStampRep, Transform, 
  Trapezoid, Wr;


VAR init_done := FALSE;

PROCEDURE FinishLinking (x: LinkInfo) =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr;
    p: RT0.ImportPtr;
    v: ADDRESS;
  BEGIN
    (* bind the module pointers *)
    m := x.modules;
    FOR i := 0 TO x.n_modules - 1 DO
      m^ := LOOPHOLE (m^, RT0.Binder)();
      INC (m, ADRSIZE (m^));
    END;

    (* initialize each module's import pointers *)
    m := x.modules;
    FOR i := 0 TO x.n_modules - 1 DO
      n := m^;
      IF (n # NIL) THEN
        p := n.import_info;
        WHILE (p # NIL) DO
          v := LOOPHOLE (p.import, RT0.ImportPtr);
          p.import := p.binder ();
          p := v;
        END;
      END;
      INC (m, ADRSIZE (m^));
    END;

    (*** NOTE: not until now were imported global variables accessible! ***)

    (* finally, patch up any "static" procedure constants *)
    m := x.modules;
    FOR i := 0 TO x.n_modules - 1 DO
      n := m^;
      IF (n = NIL) THEN
        RTMisc.FatalErrorI ("empty slot in module table ", i);
      END;
      IF (n.link # NIL) THEN n.link () END;
      INC (m, ADRSIZE (m^));
    END;
  END FinishLinking;

PROCEDURE RunMainBodies () =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr := info.modules;
  BEGIN
    FOR i := 0 TO info.n_modules - 1 DO
      n := m^;
      IF (n # NIL) AND (n.main # NIL) THEN n.main () END;
      INC (m, ADRSIZE (m^));
    END;
  END RunMainBodies;

PROCEDURE Init(x: LinkInfo) =
  BEGIN
    IF init_done THEN RETURN; END;
    init_done := TRUE;

    (* bind the module pointers and fixup each module's import pointers *)
    FinishLinking (x);

    (* spread the linker variables around where they're needed *)
    info := x;

    RT0u.nModules    := info.n_modules;
    RT0u.modules     := info.modules;

    RTHooks.bottom_of_stack := info.bottom_of_stack;
    RTHooks.top_of_stack    := info.top_of_stack;

    (* fix the runtime type structures *)
    RTTypeSRC.Init ();
   
    (* initialize the runtime *)
    RTSignal.InstallHandlers ();
    RTParams.Init ();
    RTHeapRep.Init ();
    RTThreadInit.Init ();
    RTHeapInfo.Init ();

    (* run the module main bodies *)
    RunMainBodies ();

    (* and be done *)
    (* RTProcess.Exit (0); *)
  END Init;

BEGIN
END RTMain.
