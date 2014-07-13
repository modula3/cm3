(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxIn.m3                                               *)
(* Last Modified On Thu Jan 26 14:22:57 PST 1995 By kalsow     *)

MODULE MxIn;

IMPORT Text, File, Wr, Stdio, Fmt, Word, Thread, Atom, AtomList;
IMPORT Mx, MxRep, M3ID, M3FP, M3File, MxVS, OSError;
<*FATAL Wr.Failure, Thread.Alerted*>

CONST
  End_of_buffer = '\000';
  Buffer_size   = 1024;
  N_stop_chars  = 5;  (* SPACE, NEWLINE, '*', EOB, QUOTE *)

CONST
  SignBit    = Word.LeftShift (1, 31);
  SignExtend = Word.LeftShift (Word.Not (0), 31);

TYPE
  State = RECORD
    cmd       : CHAR;
    input     : File.T        := NIL;
    errors    : Wr.T          := NIL;
    nErrors   : INTEGER       := 0;
    units     : Mx.UnitList   := NIL;
    cur_file  : Mx.File       := NIL;
    cur_unit  : Mx.Unit       := NIL;
    nameMap   : NameMap       := NIL;
    vsMap     : VSMap         := NIL;
    buf_ptr   : CARDINAL      := 0;
    buf_len   : CARDINAL      := 0;
    buf       : ARRAY [0..Buffer_size + N_stop_chars - 1] OF CHAR;
  END;

TYPE
  VSMap    = REF ARRAY OF MxVS.T;
  NameMap  = REF ARRAY OF Mx.Name;

TYPE
  CmdProc = PROCEDURE (VAR s: State): BOOLEAN RAISES {OSError.E};

EXCEPTION
  SyntaxError;

VAR
  HexDigit : ARRAY CHAR OF [0..16];
  CmdMap   : ARRAY CHAR OF CmdProc;

(*------------------------------------------------------------------------*)

PROCEDURE ReadUnits (input    : File.T;
                     filename : TEXT;
                     imported : BOOLEAN;
                     errors   : Wr.T): Mx.UnitList =
  VAR s: State;
  BEGIN
    s.input    := input;
    s.errors   := errors;
    s.cur_file := NEW (Mx.File, name := filename, imported := imported);
    s.nameMap  := NEW (NameMap, 256);
    s.vsMap    := NEW (VSMap, 4100);

    TRY
      ReadLinkFile (s);
    EXCEPT
    | OSError.E(args) => Error (s, "I/O failure", ErrMsg(args));
    | SyntaxError     => (* already reported something *)
    END;

    IF (s.nErrors > 0)
      THEN RETURN NIL
      ELSE RETURN s.units;
    END;
  END ReadUnits;

PROCEDURE ErrMsg (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    RETURN msg;
  END ErrMsg;

(*------------------------------------------------------------------------*)

PROCEDURE ReadLinkFile (VAR s: State)
  RAISES {OSError.E, SyntaxError} =
  BEGIN
    RefillBuffer (s);
    ReadMagic (s);
    LOOP
      s.cmd := s.buf [s.buf_ptr];
      INC (s.buf_ptr);
      IF CmdMap [s.cmd] (s) THEN EXIT END;
    END;
  END ReadLinkFile;

PROCEDURE ReadMagic (VAR s: State)
  RAISES {SyntaxError} =
  
  VAR MagicArr : ARRAY [0..Mx.LinkerMagicLen] OF CHAR; 
  VAR MagicText : TEXT; 
  VAR i: CARDINAL; 
  VAR ch: CHAR; 

  PROCEDURE PackageName (READONLY s: State): TEXT =  

    BEGIN
      IF s.cur_file.name = NIL 
      THEN RETURN ""
      ELSE RETURN s.cur_file.name
      END; 
    END PackageName; 

  PROCEDURE IsRTBuiltin (READONLY s: State): BOOLEAN = 
  (* Most m3linker files are produced by m3linker.  But RTBuiltin.mx is
     independently created as a source file.  However, it does not contain
     anything that depends on the range of WIDECHAR.  So newer versions of
     m3linker will accept it with either linker magic string, to obviate 
     editing it in RTBuiltin.mx.  This simplifies bootstrapping.  Compiling 
     with either sized WIDECHAR will accept either magic string in 
     RTBuiltin.mx.
  *) 
    CONST RTBuiltinString = "RTBuiltin.mx";
    CONST RTBuiltinLen = 12; (* Must be Text.Length(RTBuiltinString). *) 
    VAR ActualLen: CARDINAL;

    BEGIN
      IF s.cur_file = NIL OR s.cur_file.name = NIL THEN RETURN FALSE; END;
      ActualLen := Text.Length(s.cur_file.name); 
      IF ActualLen < RTBuiltinLen THEN RETURN FALSE END;
      IF Text.Equal 
           (Text.Sub(s.cur_file.name, ActualLen-RTBuiltinLen), RTBuiltinString) 
      THEN RETURN TRUE;
      ELSE RETURN FALSE;
      END; 
    END IsRTBuiltin; 

  BEGIN (* ReadMagic *) 
    i := 0;
    LOOP
      TRY 
        ch := GetC (s);
        IF ch = '\n' 
        THEN EXIT;
        ELSIF i > Mx.LinkerMagicLen 
        THEN 
          Error (s, "bad linkfile (excessive length header)", PackageName (s));
          RAISE SyntaxError; 
        ELSE 
          MagicArr[i] := ch;
          INC (i); 
        END;  
      EXCEPT OSError.E
      => Error (s, "bad linkfile (incomplete/missing header)", PackageName (s));
         RAISE SyntaxError; 
      END; 
    END;
    MagicText := Text.FromChars(SUBARRAY(MagicArr,0,i));

    IF Mx.UnicodeWideChar
    THEN
      IF Text.Equal(MagicText, Mx.LinkerMagicWCUni)
      THEN (* All is Ok. *)
      ELSIF Text.Equal (MagicText, Mx.LinkerMagicWC16)
      THEN (* WIDECHAR size mismatch. *)  
        IF IsRTBuiltin (s) 
        THEN (* This is Ok too.  It happens when bootstrapping. *) 
        ELSE 
          IF s.cur_file.name = NIL 
          THEN (* This is the link file for the package we are compiling. 
                  We can handle the WIDECHAR size mismatch by recompiling. *) 
            Wr.PutText 
              (Stdio.stderr, 
               "Recompiling with Unicode WIDECHAR, previously 16-bit WIDECHAR."
              ); 
            Wr.PutText (Stdio.stderr, Wr.EOL);
            Wr.Flush (Stdio.stderr);
            RAISE SyntaxError; 
          ELSE (* This is a link file of a package we are linking to. 
                  We can't handle this.  User will have to recompile it. *) 
            Wr.PutText 
              (Stdio.stderr, 
               "Compiling with Unicode WIDECHAR, but linking to 16-bit WIDECHAR: "
              ); 
            Wr.PutText (Stdio.stderr, PackageName (s));
            Wr.PutText (Stdio.stderr, Wr.EOL);
            Wr.Flush (Stdio.stderr); 
            RAISE SyntaxError; 
          END; 
        END; 
      ELSE
        Error (s, "bad linkfile (unrecognized header)", PackageName (s));
        RAISE SyntaxError; 
      END; 
    ELSE (* Compiling with 16-bit WIDECHAR. *)  
      IF Text.Equal(MagicText, Mx.LinkerMagicWC16)
      THEN (* All is Ok. *)
      ELSIF Text.Equal (MagicText, Mx.LinkerMagicWCUni)
      THEN (* WIDECHAR size mismatch. *)  
        IF IsRTBuiltin (s) 
        THEN (* This is Ok too.  It happens when reverse bootstrapping. *) 
        ELSE 
          IF s.cur_file.name = NIL 
          THEN (* This is the link file for the package we are compiling. 
                  We can handle the WIDECHAR size mismatch by recompiling. *) 
            Wr.PutText 
              (Stdio.stderr, 
               "Recompiling with 16-bit WIDECHAR, previously Unicode WIDECHAR."
              ); 
            Wr.PutText (Stdio.stderr, Wr.EOL);
            Wr.Flush (Stdio.stderr);
            RAISE SyntaxError; 
          ELSE (* This is a link file of a package we are linking to. 
                  We can't handle this.  User will have to recompile it. *)
            Wr.PutText 
              (Stdio.stderr, 
               "Compiling with 16-bit WIDECHAR, but linking to Unicode WIDECHAR: "
              );
            Wr.PutText (Stdio.stderr, PackageName (s));
            Wr.PutText (Stdio.stderr, Wr.EOL);
            Wr.Flush (Stdio.stderr);
            RAISE SyntaxError; 
          END; 
        END; 
      ELSE
        Error (s, "bad linkfile (unrecognized header)", PackageName (s));
        RAISE SyntaxError; 
      END; 
    END; 
  END ReadMagic;

PROCEDURE EndBuffer (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  BEGIN
    IF EOF (s) THEN RETURN TRUE END;
    RefillBuffer (s);
    RETURN FALSE;
  END EndBuffer;

PROCEDURE BadChar (VAR s: State): BOOLEAN =
  BEGIN
    Error (s, "unrecognized linker command: ", CharName (s.cmd));
    RETURN TRUE;
  END BadChar;

(*----------------------------------------------------------- global maps ---*)

PROCEDURE ReadName (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Nx y   --- define id number 'x' to be name 'y' *)
  VAR id := GetInteger (s, ' ');
  VAR nm := GetID      (s, '\n');
  BEGIN
    WHILE (id > LAST (s.nameMap^)) DO ExpandNameMap (s) END;
    s.nameMap [id] := nm;
    RETURN FALSE;
  END ReadName;

PROCEDURE ExpandNameMap (VAR s: State) =
  VAR n := NUMBER (s.nameMap^);  new := NEW (NameMap, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.nameMap^;
    s.nameMap := new;
  END ExpandNameMap;

PROCEDURE ReadVSInfo (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Vx a b c --- define version stamp number 'x' for symbol 'a.b' to be 'c' *)
  VAR vs : MxVS.Info;
  VAR id := GetInteger (s, ' ');
  BEGIN
    vs.source := GetName (s, ' ');
    vs.symbol := GetName (s, ' ');
    GetStamp (s, vs.stamp, '\n');

    WHILE (id > LAST (s.vsMap^)) DO ExpandVSMap (s) END;
    s.vsMap [id] := MxVS.Put (vs);
    RETURN FALSE;
  END ReadVSInfo;

PROCEDURE ExpandVSMap (VAR s: State) =
  VAR n := NUMBER (s.vsMap^);  new := NEW (VSMap, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.vsMap^;
    s.vsMap := new;
  END ExpandVSMap;

(*----------------------------------------------------------------- units ---*)

PROCEDURE ReadUnit (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* In  --- Interface 'n'  *)
  (* Mn  --- Module 'n'     *)
  VAR intf                := (s.cmd = 'I');
  VAR nm                  := GetName    (s, ' ');
  VAR n_exported_units    := GetInteger (s, ' ');
  VAR n_imported_units    := GetInteger (s, ' ');
  VAR n_imported_generics := GetInteger (s, ' ');
  VAR n_used_interfaces   := GetInteger (s, ' ');
  VAR n_used_modules      := GetInteger (s, ' ');
  VAR n_import_def_syms   := GetInteger (s, ' ');
  VAR n_import_use_syms   := GetInteger (s, ' ');
  VAR n_export_def_syms   := GetInteger (s, ' ');
  VAR n_export_use_syms   := GetInteger (s, ' ');
  VAR n_imported_types    := GetInteger (s, ' ');
  VAR n_exported_types    := GetInteger (s, ' ');
  VAR n_wishes            := GetInteger (s, '\n');
  VAR unit := NEW (Mx.Unit, name := nm, file := s.cur_file, interface := intf);
  VAR node := NEW (Mx.UnitList, unit := unit, next := s.units);
  VAR n_info := 0;
  BEGIN
    unit.exported_units.start    := n_info;  INC (n_info, n_exported_units);
    unit.imported_units.start    := n_info;  INC (n_info, n_imported_units);
    unit.imported_generics.start := n_info;  INC (n_info, n_imported_generics);
    unit.used_interfaces.start   := n_info;  INC (n_info, n_used_interfaces);
    unit.used_modules.start      := n_info;  INC (n_info, n_used_modules);
    unit.import_def_syms.start   := n_info;  INC (n_info, n_import_def_syms);
    unit.import_use_syms.start   := n_info;  INC (n_info, n_import_use_syms);
    unit.export_def_syms.start   := n_info;  INC (n_info, n_export_def_syms);
    unit.export_use_syms.start   := n_info;  INC (n_info, n_export_use_syms);
    unit.imported_types.start    := n_info;  INC (n_info, n_imported_types);
    unit.exported_types.start    := n_info;  INC (n_info, n_exported_types);
    unit.wishes.start            := n_info;  INC (n_info, n_wishes);

    unit.info  := NEW (Mx.InfoVec, n_info);
    s.units    := node;
    s.cur_unit := unit;
    RETURN FALSE;
  END ReadUnit;

PROCEDURE AddInfo (u: Mx.Unit;  VAR x: Mx.InfoList;  i: INTEGER) =
  BEGIN
    u.info [x.start + x.cnt] := i;
    INC (x.cnt);
  END AddInfo;

PROCEDURE ReadWidecharSize (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Z  --- WIDECHAR is Unicode sized. *)
  (* z  --- WIDECHAR is 16-bit. *)
  VAR Unicode: BOOLEAN; 
  BEGIN
    Unicode := s.cmd = 'Z';
    GetEOL (s); 
(* For now, silently ignore this information. *)  
    RETURN FALSE;
  END ReadWidecharSize;

PROCEDURE ReadPort (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Am  --- exports interface m *)
  (* Bm  --- imports interface m *)
  VAR export := (s.cmd = 'A');
  VAR nm     := GetName (s, '\n');
  VAR unit   := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "import/export while current unit not defined!");
    ELSIF (export) THEN
      AddInfo (unit, unit.exported_units, nm);
    ELSE (* import *)
      AddInfo (unit, unit.imported_units, nm);
    END;
    RETURN FALSE;
  END ReadPort;

PROCEDURE ReadUse (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Cm  --- uses magic info from interface m *)
  (* Dm  --- uses magic info from module m  *)
  VAR intf := (s.cmd = 'C');
  VAR nm   := GetName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "magic import while current unit not defined!");
    ELSIF (intf) THEN
      AddInfo (unit, unit.used_interfaces, nm);
    ELSE (* import *)
      AddInfo (unit, unit.used_modules, nm);
    END;
    RETURN FALSE;
  END ReadUse;

PROCEDURE ReadGeneric (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* gm    --- imports generic unit m *)
  VAR nm   := GetName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "generic import while current unit not defined!");
    ELSE
      AddInfo (unit, unit.imported_generics, nm);
    END;
    RETURN FALSE;
  END ReadGeneric;

PROCEDURE ReadVersionStamp (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* ix       --- import version stamp 'x' *)
  (* Jx       --- import & implement version stamp 'x' *)
  (* ex       --- export version stamp 'x' *)
  (* Ex       --- export & implement version stamp 'x' *)
  VAR cmd  := s.cmd;
  VAR vs   := GetVS (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "version stamps while current unit not defined!");
    ELSIF (cmd = 'J') THEN
      AddInfo (unit, unit.import_def_syms, vs);
    ELSIF (cmd = 'i') THEN
      AddInfo (unit, unit.import_use_syms, vs);
    ELSIF (cmd = 'E') THEN
      AddInfo (unit, unit.export_def_syms, vs);
    ELSE (*cmd = 'e'*)
      AddInfo (unit, unit.export_use_syms, vs);
    END;
    RETURN FALSE;
  END ReadVersionStamp;

PROCEDURE ReadRevelation (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Rn x y  --- export REVEAL 'x' = 'y'  to interface #n *)
  (* Xn x y  --- export REVEAL 'x' <: 'y' to interface #n *)
  (* rn x y  --- import REVEAL 'x' = 'y'  from interface #n *)
  (* xn x y  --- import REVEAL 'x' <: 'y' from interface #n *)
  VAR export  := (s.cmd = 'R') OR (s.cmd = 'X');
  VAR partial := (s.cmd = 'x') OR (s.cmd = 'X');
  VAR r       := NEW (Mx.Revelation, export := export, partial := partial);
  VAR unit    := s.cur_unit;
  BEGIN
    r.source := GetName (s, ' ');
    r.lhs    := GetTypeName (s, ' ');
    r.rhs    := GetTypeName (s, '\n');
    IF (unit = NIL) THEN
      Error (s, "revelations while current unit not defined!");
    ELSE
      r.next := unit.revelations;
      unit.revelations := r;
    END;
    RETURN FALSE;
  END ReadRevelation;

PROCEDURE ReadWish (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* wt       --- wish to know the object type 't'. *)
  VAR type := GetTypeName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "object wish while current unit not defined!");
    ELSE
      AddInfo (unit, unit.wishes, type);
    END;
    RETURN FALSE;
  END ReadWish;

PROCEDURE ReadType (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* tx       --- import type 'x'   *)
  (* Tx       --- export type 'x'   *)
  VAR cmd  := s.cmd;
  VAR type := GetTypeName (s, '\n');
  VAR unit := s.cur_unit;
  BEGIN
    IF (unit = NIL) THEN
      Error (s, "type import/export while current unit not defined!");
    ELSIF (cmd = 't') THEN
      AddInfo (unit, unit.imported_types, type);
    ELSE
      AddInfo (unit, unit.exported_types, type);
    END;
    RETURN FALSE;
  END ReadType;

PROCEDURE ReadTypeId (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* na x     --- a is a name for type (UID) x.  *)
  VAR TypeId: Mx.Name; 
  VAR type: Mx.TypeName;
  BEGIN
    TypeId := GetName (s, ' '); 
    type := GetTypeName (s, '\n');
(* For now, silently ignore this information. *) 
    RETURN FALSE;
  END ReadTypeId; 

PROCEDURE ReadObjectType (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* on t s ds da ms -- import object type from interface unit n    *)
  (* pn t s ds da ms -- import object type from module unit n       *)
  (* Ot s ds da ms   -- export object type 't' with supertype 's',  *)
  (*                    data size 'ds', data alignment 'da', and    *)
  (*                    method size 'ms' from unit #n               *)
  VAR export := (s.cmd = 'O');
  VAR module := (s.cmd = 'p');
  VAR obj    := NEW (Mx.ObjectType, export := export, from_module := module);
  VAR unit   := s.cur_unit;
  BEGIN
    IF (NOT export) THEN  obj.source := GetName (s, ' ') END;
    obj.type          := GetTypeName (s, ' ');
    obj.super_type    := GetTypeName (s, ' ');
    obj.data_size     := GetInteger (s, ' ');
    obj.data_align    := GetInteger (s, ' ');
    obj.method_size   := GetInteger (s, '\n');

    IF (unit = NIL) THEN
      Error (s, "object info while current unit not defined!");
    ELSIF (export) THEN
      obj.source := unit.name;
      obj.from_module := NOT unit.interface;
      obj.next := unit.exported_objects;
      unit.exported_objects := obj;
    ELSE
      obj.next := unit.imported_objects;
      unit.imported_objects := obj;
    END;

    RETURN FALSE;
  END ReadObjectType;

PROCEDURE ReadOpaqueType (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  (* Qt s     --- define opaque type 't' with supertype 's'. *)
  (* qt s n   --- define opaque type 't' with supertype 's', named n. *)
  VAR cmd := s.cmd;
  VAR opaque := NEW (Mx.OpaqueType);
  VAR unit := s.cur_unit;
  VAR TypeName: Mx.Name;
  BEGIN
    IF cmd = 'q' THEN
      opaque.type       := GetTypeName (s, ' ');
      opaque.super_type := GetTypeName (s, ' ');
      TypeName := GetName (s, '\n');
      opaque.TypeName := TypeName; 
    ELSE (* 'Q' *)  
      opaque.type       := GetTypeName (s, ' ');
      opaque.super_type := GetTypeName (s, '\n');
      opaque.TypeName := M3ID.NoID; 
    END; 

    IF (unit = NIL) THEN
      Error (s, "opaque type defined while current unit not defined!");
    ELSE
      opaque.next := unit.opaques;
      unit.opaques := opaque;
    END;

    RETURN FALSE;
  END ReadOpaqueType;

PROCEDURE SkipComment (VAR s: State): BOOLEAN
  RAISES {OSError.E} =
  VAR ch := '/';
  BEGIN
    WHILE (ch # '\n') DO ch := GetC (s) END;
    RETURN FALSE;
  END SkipComment;

PROCEDURE SkipBlank (<*UNUSED*> VAR s: State): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END SkipBlank;

PROCEDURE GetName (VAR s: State;  term: CHAR): Mx.Name
  RAISES {OSError.E} =
  VAR id := GetInteger (s, term);
  BEGIN
    IF (0 <= id) AND (id < NUMBER (s.nameMap^)) THEN
      RETURN s.nameMap [id];
    ELSE
      Error (s, "bad unit number: " & Fmt.Int (id));
      RETURN M3ID.NoID;
    END;
  END GetName;

PROCEDURE GetVS (VAR s: State;  term: CHAR): MxVS.T
  RAISES {OSError.E} =
  VAR id := GetInteger (s, term);
  BEGIN
    IF (0 <= id) AND (id < NUMBER (s.vsMap^)) THEN
      RETURN s.vsMap [id];
    ELSE
      Error (s, "bad version stamp number: " & Fmt.Int (id));
      RETURN MxVS.NoVS;
    END;
  END GetVS;

PROCEDURE GetInteger (VAR s: State;  term: CHAR): INTEGER
  RAISES {OSError.E} =
  VAR n   := 0;
  VAR len := 0;
  VAR ch: CHAR;
  BEGIN
    LOOP
      ch := s.buf [s.buf_ptr];
      INC (s.buf_ptr);
      IF (ch < '0') OR ('9' < ch) THEN
        (* NOTE: none of the stop characters are legal digits *)
        IF (s.buf_ptr <= s.buf_len) THEN EXIT END;
        ch := GetC (s);
        IF (ch < '0') OR ('9' < ch) THEN EXIT END;
      END;
      n := 10 * n + (ORD (ch) - ORD ('0'));
      INC (len);
    END;
    IF (ch = '\r') THEN ch := GetC (s); END;
    IF (len <= 0)  THEN Error (s, "expected integer") END;
    IF (ch # term) THEN Error (s, "expecting separator after integer") END;
    RETURN n;
  END GetInteger;

PROCEDURE GetEOL (VAR s: State)
  RAISES {OSError.E} =
  VAR ch: CHAR;
  BEGIN
    ch := s.buf [s.buf_ptr];
    INC (s.buf_ptr);
    IF (ch = '\r') THEN ch := GetC (s); END;
    IF (ch # '\n') THEN Error (s, "expecting end of line.") END;
  END GetEOL;

PROCEDURE GetTypeName (VAR s: State;  term: CHAR): Mx.TypeName
  RAISES {OSError.E} =
  VAR n   := 0;
  VAR len := 0;
  VAR ch    : CHAR;
  VAR digit : INTEGER;
  BEGIN
    LOOP
      ch := s.buf [s.buf_ptr];
      INC (s.buf_ptr);
      digit := HexDigit [ch];
      IF (digit > 15) THEN
        (* NOTE: none of the stop characters are legal digits *)
        IF (s.buf_ptr <= s.buf_len) THEN EXIT END;
        ch := GetC (s);
        digit := HexDigit [ch];
        IF (digit > 15) THEN EXIT END;
      END;
      n := Word.Plus (Word.Times (n, 16), digit);
      INC (len);
    END;
    IF (ch = '\r') THEN ch := GetC (s); END;
    IF (len <= 0)  THEN Error (s, "expected typename") END;
    IF (ch # term) THEN Error (s, "expecting separator after typename") END;
    IF Word.And (n, SignBit) # 0 THEN  n := Word.Or (SignExtend, n);  END;
    RETURN n;
  END GetTypeName;

PROCEDURE GetID (VAR s: State;  term: CHAR): Mx.Name
  RAISES {OSError.E} =
(* Note: we don't need to check for array overruns since all calls
   to GetString include a terminating character that's in the "stop set"
   at the end of the buffer *)
  VAR stop, start, len: CARDINAL;  overflow: TEXT;  ch: CHAR;
  BEGIN
    start := s.buf_ptr;
    stop  := start;
    LOOP
      ch := s.buf[stop];
      IF (ch = '\r') THEN EXIT END;
      IF (ch = term) THEN EXIT END;
      INC (stop)
    END;

    IF (stop < s.buf_len) THEN
      (* this is the simple case, the string's entirely in the buffer *)
      s.buf_ptr := stop + 1;
      RETURN M3ID.FromStr (SUBARRAY (s.buf,start,stop-start));
    END;

    overflow := "";
    LOOP
      (* we've overrun the end of the buffer *)
      (* save the current string & refill the buffer *)
      len := MAX (s.buf_len - start, 0);
      overflow := overflow & Text.FromChars (SUBARRAY (s.buf, start, len));
      RefillBuffer (s);
      start := 0;
      stop := 0;

      IF EOF (s) THEN
        RETURN M3ID.Add (overflow);
      END;

      LOOP
        ch := s.buf[stop];
        IF (ch = '\r') THEN EXIT END;
        IF (ch = term) THEN EXIT END;
        INC (stop)
      END;

      IF (stop < s.buf_len) THEN
        (* we terminated inside the buffer *)
        s.buf_ptr := stop + 1;
        len := stop - start;
        overflow := overflow & Text.FromChars (SUBARRAY (s.buf, start, len));
        RETURN M3ID.Add (overflow);
      END;
    END;

  END GetID;

PROCEDURE GetStamp (VAR s: State;  VAR x: M3FP.T;  term: CHAR)
  RAISES {OSError.E} =
  VAR
    ch: CHAR;
    i, j, d1, d0: INTEGER;
    len: INTEGER := 0;
    buf: ARRAY [0..15] OF CHAR;
  BEGIN
    LOOP
      ch := GetC (s);
      IF (ch = term) THEN EXIT END;
      IF (len >= NUMBER (buf)) THEN
        Error (s, "version stamp too long!"); EXIT;
      ELSIF (len < NUMBER (buf)) THEN
        buf[len] := ch;
      END;
      INC (len);
    END;
    len := MIN (len, NUMBER (buf));

    (* convert the buffered characters into a fingerprint *)
    i := 0;  j := 0;
    WHILE (i < len) DO
      d1 := HexDigit [buf[i]];  INC (i);
      d0 := HexDigit [buf[i]];  INC (i);
      IF (d1 > 15) OR (d0 > 15) THEN
        Error (s, "illegal hex digit in version stamp");
      END;
      x.byte[j] := Word.LeftShift (d1, 4) + d0;  INC (j);
    END;

    WHILE (j <= LAST (x.byte)) DO
      x.byte[j] := 0;  INC (j);
    END;
  END GetStamp;

(*------------------------------------------------------------------------*)

PROCEDURE GetC (VAR s: State): CHAR
  RAISES {OSError.E} =
  VAR c: CHAR;
  BEGIN
    REPEAT
      IF (s.buf_ptr >= s.buf_len) THEN RefillBuffer (s) END;
      c := s.buf [s.buf_ptr];
      INC (s.buf_ptr);
    UNTIL (c # '\r');   (* ignore all return characters *)
    RETURN c;
  END GetC;

PROCEDURE EOF (VAR s: State): BOOLEAN =
  BEGIN
    RETURN (s.buf_ptr >= s.buf_len) AND (s.buf_len = 0);
  END EOF;

PROCEDURE RefillBuffer (VAR s: State)
  RAISES {OSError.E} =
  BEGIN
    s.buf_len := M3File.Read (s.input, s.buf, Buffer_size);
    s.buf [s.buf_len + 0] := End_of_buffer;
    s.buf [s.buf_len + 1] := ' ';
    s.buf [s.buf_len + 2] := '\n';
    s.buf [s.buf_len + 3] := '*';
    s.buf [s.buf_len + 4] := '\"';
    s.buf_ptr := 0;
  END RefillBuffer;

(*------------------------------------------------------------------------*)

PROCEDURE Error (VAR s: State;  a, b, c, d: Text.T := NIL) =
  BEGIN
    INC  (s.nErrors);
    IF (s.errors = NIL) THEN RETURN END;
    IF (s.cur_file # NIL) AND (s.cur_file.name # NIL) THEN
      Wr.PutText (s.errors, s.cur_file.name);
      IF (s.cur_unit # NIL) THEN
        Wr.PutText (s.errors, " (");
        Wr.PutText (s.errors, MxRep.UnitName (s.cur_unit));
        Wr.PutText (s.errors, ")");
      END;
      Wr.PutText (s.errors, ": ");
    END;
    Wr.PutText (s.errors, "ERROR: ");
    IF (a # NIL) THEN Wr.PutText (s.errors, a); END;
    IF (b # NIL) THEN Wr.PutText (s.errors, b); END;
    IF (c # NIL) THEN Wr.PutText (s.errors, c); END;
    IF (d # NIL) THEN Wr.PutText (s.errors, d); END;
    Wr.PutText (s.errors, Wr.EOL);
    Wr.Flush (s.errors); 
  END Error;

PROCEDURE CharName (c: CHAR): Text.T =
  BEGIN
    IF (' ' <= c) AND (c <= '~')
      THEN RETURN "\'" & Text.FromChar (c) & "\'" ;
      ELSE RETURN "\'\\" & Fmt.Pad (Fmt.Int (ORD (c), 8), 3, '0') & "\'" ;
    END;
  END CharName;


PROCEDURE Init () =
  BEGIN
    FOR i := FIRST (HexDigit) TO LAST (HexDigit) DO HexDigit[i] := 16 END;
    FOR i := '0' TO '9' DO HexDigit [i] := ORD (i) - ORD ('0') END;
    FOR i := 'a' TO 'f' DO HexDigit [i] := ORD (i) - ORD ('a') + 10 END;
    FOR i := 'A' TO 'F' DO HexDigit [i] := ORD (i) - ORD ('A') + 10 END;

    FOR c := FIRST (CmdMap) TO LAST (CmdMap) DO CmdMap[c] := BadChar END;
    CmdMap ['N'] := ReadName;
    CmdMap ['V'] := ReadVSInfo;
    CmdMap ['I'] := ReadUnit;
    CmdMap ['M'] := ReadUnit;
    CmdMap ['A'] := ReadPort;
    CmdMap ['B'] := ReadPort;
    CmdMap ['C'] := ReadUse;
    CmdMap ['D'] := ReadUse;
    CmdMap ['g'] := ReadGeneric;
    CmdMap ['i'] := ReadVersionStamp;
    CmdMap ['J'] := ReadVersionStamp;
    CmdMap ['e'] := ReadVersionStamp;
    CmdMap ['E'] := ReadVersionStamp;
    CmdMap ['R'] := ReadRevelation;
    CmdMap ['X'] := ReadRevelation;
    CmdMap ['r'] := ReadRevelation;
    CmdMap ['x'] := ReadRevelation;
    CmdMap ['t'] := ReadType;
    CmdMap ['T'] := ReadType;
    CmdMap ['w'] := ReadWish;
    CmdMap ['o'] := ReadObjectType;
    CmdMap ['p'] := ReadObjectType;
    CmdMap ['O'] := ReadObjectType;
    CmdMap ['Q'] := ReadOpaqueType;
    CmdMap ['q'] := ReadOpaqueType;
    CmdMap ['n'] := ReadTypeId;
    CmdMap ['Z'] := ReadWidecharSize;
    CmdMap ['z'] := ReadWidecharSize;
    CmdMap ['/'] := SkipComment;
    CmdMap [' '] := SkipBlank;
    CmdMap ['\r'] := SkipBlank;
    CmdMap ['\t'] := SkipBlank;
    CmdMap ['\n'] := SkipBlank;
    CmdMap [End_of_buffer] := EndBuffer;
  END Init;

BEGIN
  Init ();
END MxIn.
