(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE Main;

IMPORT ASCII, FileRd, Fmt, IntRefTbl, Lex, M3AST, M3Const, M3ID, M3Lexer, M3Type;
IMPORT OSError, Params, Pathname, Process, RefList, Target;
IMPORT Rd, Stdio, Text, TextList, Thread, Wr;
IMPORT DLL, GlueObj, LibFile;

TYPE
  ExportInfo = REF RECORD
    name   : M3ID.T;  (* symbol name *)
    file   : TEXT;    (* defining DLL *)
    ord    : INTEGER; (* ordinal export value *)
    offset : INTEGER; (* code offset in the DLL *)
    match  : ProcInfo;
  END;

TYPE
  ProcInfo = REF RECORD
    name  : M3ID.T;   (* external procedure name *)
    cconv : M3ID.T;   (* calling convention name *)
    type  : M3Type.T; (* signature type *)
    intf  : IntfInfo; (* defining interface *)
    node  : M3AST.NodeIndex; (* proc's declaration in interface's ast *)
  END;

TYPE
  IntfInfo = REF RECORD
    name : M3ID.T;
    file : TEXT;
    ast  : M3AST.T;
    err  : BOOLEAN;
  END;

VAR
  procs   := NEW (IntRefTbl.Default).init();  (* proc name -> ProcInfo *)
  intfs   := NEW (IntRefTbl.Default).init();  (* interface name -> IntfInfo *)
  files   : TextList.T := NIL;
  direct_calls : BOOLEAN := FALSE;

PROCEDURE DoIt () =
  BEGIN
    IF NOT Target.Init ("NT386") THEN
      Die ("*** unable to initialize Target: NT386");
    END;
    ParseCmdLine ();
    ScanFiles ();
    InhaleInterfaces ();
    ProcessDLLs ();
  END DoIt;

PROCEDURE ParseCmdLine () =
  VAR arg: TEXT;
  BEGIN
    FOR i := 1 TO Params.Count - 1 DO
      ParseArg (Params.Get (i));
    END;
    files := TextList.ReverseD (files);
  END ParseCmdLine;

PROCEDURE ParseCmdFile (file: TEXT) =
  VAR rd := OpenRd (file);
  BEGIN
    IF (rd = NIL) THEN RETURN; END;

    (* treat each non-blank field as a separate command line argument *)
    TRY
      TRY
        WHILE NOT Rd.EOF (rd) DO
          Lex.Skip (rd, Lex.Blanks);
          ParseArg (Lex.Scan (rd, Lex.NonBlanks));
        END;
      FINALLY
        Rd.Close (rd);
      END;
    EXCEPT Rd.Failure, Thread.Alerted =>
      (* bail out... *)
    END;
  END ParseCmdFile;

PROCEDURE ParseArg (arg: TEXT) =
  BEGIN
    IF Text.Equal (arg, "-direct") THEN
      direct_calls := TRUE;
    ELSIF Text.Equal (Text.Sub (arg, 0, 2), "-F") THEN
      ParseCmdFile (Text.Sub (arg, 2));
    ELSE
      files := TextList.Cons (arg, files);
    END;
  END ParseArg;

PROCEDURE ScanFiles () =
  VAR
    file, ext: TEXT;
    ref: REFANY;
    intf, xx: IntfInfo;
    x := files;
  BEGIN
    WHILE (x # NIL) DO
      file := x.head;  x := x.tail;
      IF Text.Length (file) > 0 THEN
        ext  := Pathname.LastExt (file);
        IF FileNameEq (ext, "i3") THEN
          intf := NEW (IntfInfo, file := file, ast := NIL, err := FALSE);
          intf.name := M3ID.Add (Pathname.LastBase (file));
          IF intfs.get (intf.name, ref) THEN
            xx := ref;
            Out ("*** duplicate interface: ", M3ID.ToText (intf.name));
            Out ("***    ", xx.file, " used and");
            Out ("***    ", file, " ignored.");
          ELSE
            EVAL intfs.put (intf.name, intf);
          END;
        ELSIF FileNameEq (ext, "DLL") THEN
          (* skip for now *)
        ELSE
          Out ("*** unrecognized file type: ", file, ", ignored");
        END;
      END;
    END;
  END ScanFiles;

PROCEDURE InhaleInterfaces () =
  VAR iter := intfs.iterate ();   nm: INTEGER;  ref: REFANY;
  BEGIN
    WHILE iter.next (nm, ref) DO
      InhaleInterface (ref);
    END;
  END InhaleInterfaces;

PROCEDURE ProcessDLLs () =
  VAR file, ext: TEXT;  x := files;
  BEGIN
    WHILE (x # NIL) DO
      file := x.head;  x := x.tail;
      IF Text.Length (file) > 0 THEN
        ext  := Pathname.LastExt (file);
        IF FileNameEq (ext, "i3") THEN
          (* skip this pass *)
        ELSIF FileNameEq (ext, "DLL") THEN
          ProcessDLL (file);
        ELSE
          Out ("*** unrecognized file type: ", file, ", ignored");
        END;
      END;
    END;
  END ProcessDLLs;

(*------------------------------------------------------------------ DLLs ---*)

TYPE
  State = RECORD
    dll_file : TEXT;
    dll_name : TEXT;
    lib_file : TEXT;
    head_obj : LibFile.Obj;
    tail_obj : LibFile.Obj;
    exports  : IntRefTbl.T;  (* export name -> ExportInfo *)
  END;

PROCEDURE ProcessDLL (file: TEXT) =
  VAR s: State;
  BEGIN
    s.dll_file := file;
    s.dll_name := Pathname.LastBase (file);
    s.lib_file := Pathname.Join (NIL, s.dll_name, "LIB");
    s.head_obj := NIL;
    s.tail_obj := NIL;
    s.exports  := NEW (IntRefTbl.Default).init();  (* export name -> ExportInfo *)

    Out ("");
    Out ("reading ", s.dll_name, "...");
    InhaleDLL (s);
    AddObj (s, GlueObj.GenHeader1 (s.dll_name));
    AddObj (s, GlueObj.GenHeader2 (s.dll_name));
    AddObj (s, GlueObj.GenHeader3 (s.dll_name));
    MatchDLLExports (s);

    Out ("writing ", s.lib_file, "...");
    LibFile.Gen (s.lib_file, s.head_obj);
  END ProcessDLL;

PROCEDURE InhaleDLL (VAR s: State) =
  VAR
    exp   : DLL.ExportList;
    id    : M3ID.T;
    ref   : REFANY;
    einfo : ExportInfo;
  BEGIN
    TRY
      exp := DLL.GetExports (s.dll_file);
    EXCEPT DLL.Error (msg) =>
      Out ("*** file ", s.dll_file, ": ", msg);
      RETURN;
    END;

    IF (exp = NIL) THEN
      Out ("*** no exports found in ", s.dll_file);
      RETURN;
    END;

    FOR i := 0 TO LAST (exp^) DO
      WITH z = exp[i] DO
        IF (z.name # NIL) THEN
          id := M3ID.Add (z.name);
          IF s.exports.get (id, ref) THEN
            einfo := ref;
            Out ("*** duplicate DLL export: ", z.name, ", ignored");
            Out ("***    in ", einfo.file, " and ", s.dll_file);
          ELSE
            EVAL s.exports.put (id, NEW (ExportInfo, name := id, file := s.dll_file,
                                         ord := z.ord, offset := z.offset));
          END;
        END;
      END;
    END;
  END InhaleDLL;

PROCEDURE MatchDLLExports (VAR s: State) =
  VAR
    iter := s.exports.iterate ();
    nm   : INTEGER;
    ref  : REFANY;
    exp  : ExportInfo;
    proc : ProcInfo;
    n_matched := 0;
    n_missed  := 0;
    args : ArgInfo;
    missed : RefList.T := NIL;
    checks : TEXT;
  BEGIN
    Out ("                                 # arg");
    Out ("matched procedure                bytes  checked parameter offsets");
    Out ("---------------------------      -----  -------------------------------");
    WHILE iter.next (nm, ref) DO
      exp := NARROW (ref, ExportInfo);
      proc := FindProc (exp.name);
      IF (proc # NIL) THEN
        CheckArgs (proc.type, args);

        checks := "";
        FOR i := 0 TO args.n_checks-1 DO
          checks := checks & "  " & Fmt.Int (args.checks[i]);
        END;
        Out (Fmt.Pad (M3ID.ToText (exp.name), 33, ' ', Fmt.Align.Left),
             Fmt.Pad (Fmt.Int (args.n_bytes), 5, ' ', Fmt.Align.Right), checks);

        AddObj (s, GlueObj.Gen (s.dll_name, M3ID.ToText (exp.name),
                                args.std_call, exp.ord, args.n_bytes,
                                args.n_checks, args.checks, direct_calls));
        INC (n_matched);
      ELSE
        INC (n_missed);
        missed := RefList.Cons (exp, missed);
      END;
    END;

    IF (missed # NIL) THEN
      Out ();
      Out ("                                 DLL");
      Out ("unmatched procedure              ord#  code offset");
      Out ("---------------------------      ----  -----------");
      WHILE (missed # NIL) DO
        exp := missed.head;  missed := missed.tail;
        Out (Fmt.Pad (M3ID.ToText (exp.name), 33, ' ', Fmt.Align.Left),
             Fmt.Pad (Fmt.Int (exp.ord), 4, ' ', Fmt.Align.Right),
             "  16_", Fmt.Unsigned (exp.offset));
      END;
    END;

    Out ();
    Out (Fmt.Int (n_matched), " exports matched.");
    Out (Fmt.Int (n_missed),  " exports unmatched.");
    Out ();
  END MatchDLLExports;

PROCEDURE AddObj (VAR s: State;  obj: LibFile.Obj) =
  BEGIN
    IF s.head_obj = NIL
      THEN s.head_obj := obj;
      ELSE s.tail_obj.next := obj;
    END;
    s.tail_obj := obj;
  END AddObj;

PROCEDURE FindProc (nm: M3ID.T): ProcInfo =
  VAR p: ProcInfo;  ref: REFANY;
  BEGIN
    IF NOT procs.get (nm, ref) THEN RETURN NIL; END;
    p := NARROW (ref, ProcInfo);
    RETURN p;
  END FindProc;

TYPE
  ArgInfo = RECORD
    std_call : BOOLEAN;
    n_bytes  : INTEGER;
    n_checks : INTEGER;
    checks   : ARRAY [0..63] OF INTEGER;
  END;

VAR
  StdCall: Target.CallingConvention := NIL;

PROCEDURE CheckArgs (tipe: M3Type.T;  VAR args: ArgInfo) =
  (* NOTE: We ASSUME that Win32 doesn't really traffic in Modula-3
     OBJECTs or traced REFs.  Otherwise, we'd need to parameters
     of those types too....  *)
  VAR info : M3Type.Info;  arg_words, arg_bytes: INTEGER;
  BEGIN
    args.std_call := TRUE;
    args.n_bytes  := 0;
    args.n_checks := 0;
    TYPECASE tipe OF
    | NULL => (*skip*)
    | M3Type.Procedure (proc) =>
        IF proc.callingConv # NIL THEN
          IF (StdCall = NIL) THEN
            StdCall := Target.FindConvention ("WINAPI");
          END;
          IF (StdCall # NIL) THEN
            args.std_call := (proc.callingConv.m3cg_id = StdCall.m3cg_id);
          END;
        END;
        IF proc.formals # NIL THEN
          FOR i := 0 TO LAST (proc.formals^) DO
            WITH z = proc.formals[i] DO
              IF z.mode # M3Type.Mode.Value THEN
                args.checks [args.n_checks] := args.n_bytes;
                INC (args.n_checks);
                info.size := Target.Address.size;
              ELSE
                M3Type.GetInfo (z.type, info);
                CASE info.class OF
                | M3Type.Class.Object =>
                    SuspiciousArg (z);
                    args.checks [args.n_checks] := args.n_bytes;
                    INC (args.n_checks);
                | M3Type.Class.Opaque =>
                    IF info.is_traced THEN
                      SuspiciousArg (z);
                      args.checks [args.n_checks] := args.n_bytes;
                      INC (args.n_checks);
                    ELSE
                      (* assume these are Win32 HANDLEs that cannot be checked *)
                    END;
                | M3Type.Class.Ref =>
                    IF info.is_traced THEN SuspiciousArg (z); END;
                    args.checks [args.n_checks] := args.n_bytes;
                    INC (args.n_checks);
                | M3Type.Class.Unknown =>
                    Out ("*** bad formal type: formal = ", M3ID.ToText (z.name),
                         " => ", info.err_msg);
                    info.size := Target.Address.size;
                ELSE
                    (* nothing to do *)
                END;
              END;
            END;
            arg_words := info.size + Target.Address.size - 1;
            arg_words := arg_words DIV Target.Address.size;
            arg_bytes := arg_words * (Target.Address.size DIV Target.Byte);
            INC (args.n_bytes, arg_bytes);
          END;
        END;
    ELSE (*skip*)
    END;
  END CheckArgs;

PROCEDURE SuspiciousArg (READONLY z: M3Type.FormalDesc) =
  BEGIN
    Out ("*** suspicious formal parameter: ", M3ID.ToText (z.name));
  END SuspiciousArg;

(*------------------------------------------------------------- .i3 files ---*)

PROCEDURE InhaleInterface (intf: IntfInfo) =
  VAR
    ast    := GetAST (intf);
    pinfo  : ProcInfo;
    ref    : REFANY;
    nm     : M3ID.T;
    cc     : M3ID.T;
    tipe   : M3Type.T;
  BEGIN
    IF (ast = NIL) OR (ast.nodes = NIL) THEN RETURN END;
    FOR i := 0 TO LAST (ast.nodes^) DO
      WITH z = ast.nodes[i] DO
        IF (z.op = M3AST.OP_ProcDecl) AND (z.info # M3ID.NoID) THEN
(***
Out ("proc: ", M3ID.ToText (z.info));
***)
          IF GetExternInfo (ast, i, nm, cc, tipe) THEN
            IF procs.get (nm, ref) THEN
              pinfo := ref;
              Out ("*** duplicate procedure: ", M3ID.ToText (nm), ", ignored");
              Out ("***    in ", pinfo.intf.file, " and ", intf.file);
            ELSE
              EVAL procs.put (nm, NEW (ProcInfo, name := nm,
                                       cconv := cc, type := tipe,
                                       intf := intf, node := i));
            END;
          END;
        END;
      END;
    END;
  END InhaleInterface;

PROCEDURE GetExternInfo (ast: M3AST.T;  decl: M3AST.NodeIndex;
                         VAR(*OUT*) alias, cc: M3ID.T;
                         VAR(*OUT*) tipe: M3Type.T): BOOLEAN =
  VAR
    ch        : ARRAY [0..7] OF M3AST.NodeIndex;
    n_ch      := M3AST.GetChildren (ast, decl, ch);
    is_extern := FALSE;
    type_node := ch[0];
    attr_node := ch[1];
    value     : M3Const.T;
  BEGIN
    IF (n_ch <= 1) THEN RETURN FALSE; END;
    IF (ast.nodes[attr_node].op # M3AST.OP_Attributes) THEN RETURN FALSE; END;

    (* set the default return values *)
    alias := ast.nodes[decl].info;
    cc    := M3ID.NoID;
    tipe  := NIL;

    (* check the attributes *)
    n_ch := M3AST.GetChildren (ast, ch[1], ch);
    FOR i := 0 TO n_ch-1 DO
      WITH z = ast.nodes[ch[i]] DO
        IF    (z.op = M3AST.OP_External) THEN  is_extern := TRUE;
        ELSIF (z.op = M3AST.OP_Alias)    THEN  alias := z.info;
        ELSIF (z.op = M3AST.OP_CallConv) THEN  cc := z.info;
        END;
      END;
    END;
    IF NOT is_extern THEN RETURN FALSE; END;

(***
Out ("  eval type @ ", Fmt.Int (type_node));
***)
    TRY
      M3Const.Eval (ast, type_node, importer, value)
    EXCEPT M3Const.Error (msg) =>
      Out ("*** error: TYPEOF(", M3ID.ToText (alias), ") => ", msg);
      RETURN FALSE;
    END;

    IF (value.class # M3Const.Class.Type) THEN
      Out ("*** error: TYPEOF(", M3ID.ToText (alias), ") => not a type");
      RETURN FALSE;
    END;

    tipe := value.type;
    RETURN TRUE;
  END GetExternInfo;

VAR
  importer := NEW (M3Const.ImportOracle, find := FindInterface);

PROCEDURE FindInterface (<*UNUSED*> self: M3Const.ImportOracle;
                         nm: M3ID.T): M3AST.T =
  VAR intf: IntfInfo;  ref: REFANY;
  BEGIN
    IF NOT intfs.get (nm, ref) THEN
      Out ("*** unable to find import: ", M3ID.ToText (nm));
      RETURN NIL;
    END;
    intf := ref;
    IF (intf.ast = NIL) AND (NOT intf.err) THEN
      EVAL GetAST (intf);
    END;
    IF (intf.ast = NIL) THEN
      Out ("*** imported ast contains errors: ", M3ID.ToText (nm));
    END;
    RETURN intf.ast;
  END FindInterface;

VAR
  n_parse_errs := 0;
  parse_intf   : IntfInfo := NIL;

PROCEDURE GetAST (intf: IntfInfo): M3AST.T =
  VAR
    rd   := OpenRd (intf.file);
    scan := NEW (M3Lexer.T).initFromRd (rd);
    ast  : M3AST.T;
    save := parse_intf;
  BEGIN
Out ("parsing ", intf.file, "...");
    n_parse_errs := 0;
    parse_intf := intf;
    ast := M3AST.Parse (scan, ParseErr);
    CloseRd (rd);
    IF (n_parse_errs > 0) THEN ast := NIL; END;
    intf.ast := ast;
    parse_intf := save;
    RETURN ast;
  END GetAST;

PROCEDURE ParseErr (msg: TEXT;  lex: M3Lexer.T): BOOLEAN =
   BEGIN
     parse_intf.err := TRUE;
     Out ("*** file \"" & parse_intf.file & ", line " & Fmt.Int (lex.line)
           & ", col " & Fmt.Int (lex.column), ": ", msg);
     INC (n_parse_errs);
     RETURN FALSE;
   END ParseErr;

(*-------------------------------------------------------- misc utilities ---*)

PROCEDURE FileNameEq (a, b: TEXT): BOOLEAN =
  VAR len: INTEGER;  c1, c2: CHAR;
  BEGIN
    IF (a = NIL) OR (b = NIL) THEN RETURN (a = b); END;
    len := Text.Length (a);
    IF (Text.Length (b) # len) THEN RETURN FALSE; END;
    FOR i := 0 TO len-1 DO
      c1 := ASCII.Upper[Text.GetChar (a, i)];
      c2 := ASCII.Upper[Text.GetChar (b, i)];
      IF (c1 # c2) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END FileNameEq;

PROCEDURE Die (a, b, c, d: TEXT := NIL) =
  BEGIN
    Out (a, b, c, d);
    Process.Exit (1);
  END Die;

PROCEDURE Out (a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    Wr.PutText (wr, Wr.EOL);
    Wr.Flush (wr);
  END Out;

PROCEDURE OpenRd (nm: TEXT): Rd.T =
  BEGIN
    TRY
      RETURN FileRd.Open (nm);
    EXCEPT OSError.E =>
      Die ("*** unable to open \"", nm, "\"");
      RETURN NIL;
    END;
  END OpenRd;

PROCEDURE CloseRd (rd: Rd.T) =
  BEGIN
    TRY Rd.Close (rd);  EXCEPT Rd.Failure, Thread.Alerted => (* skip *) END;
  END CloseRd;

BEGIN
  DoIt ();
END Main.
