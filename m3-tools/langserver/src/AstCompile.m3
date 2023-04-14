(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

MODULE AstCompile;

(* The editor programm text is run through the m3tk compiler to generate
   errors and warnings to be sent back to the client to be displayed. *)

IMPORT Rd, NullWr, Pathname, FS, File, TextSeq, RefSeq, MxConfig;
IMPORT M3Args, M3Context, M3AST_AS, M3CUnit, M3CFETool, M3ErrorStream;
IMPORT M3ToolFrame, M3CGoList, M3PathTool;
IMPORT M3CWarnTool, M3Error, TextRd, Text;
IMPORT M3AST_all;                (* this cannot be omitted; it defines the
                                    particular revelations for all the AST
                                    nodes *)
IMPORT Debug;

<*FATAL ANY *>

TYPE
  RAT = REF ARRAY OF TEXT;

  (* when we create this save the fqn of the module ie full path in name
     plus the stream from the client (vscode, emacs or whatever) *)
  StreamerClosure = M3CGoList.Streamer OBJECT
                      name  : TEXT;
                      stream: Rd.T;
                    OVERRIDES
                      getStream := GetStream;
                    END;

REVEAL
  WorkerClosure = WorkerClosurePublic BRANDED OBJECT
    feTool, warnTool, pathTool: M3Args.T; (* the tools *)
    uri : TEXT;                    (* the uri of the module we are editing *)
    prefix : TEXT;                 (* the path of the module we are editing *)
    text : TEXT;                   (* the text of the module with all changes*)
    unit : TEXT;                   (* Basename of module or interface *)
    targetDir : TEXT;              (* target dir where .M3IMPTAB is located *)
    context  : M3Context.T;        (* the saved context *)
    dirs : TextSeq.T;              (* search paths as a seq *)
    compType : CompType;           (* module interface or generic form *)
    errSeq : RefSeq.T;             (* error seq direct from m3tk *)
    errWr : NullWr.T;
  METHODS
    setup() := Setup;
    getDirs(name : TEXT) := GetDirs;
  OVERRIDES
    init := Init;
    getContext := GetContext;
    compile := Compile;
    fileName := FileName;
    path := Path;
    getUnit := GetUnit;
    work := Work;
  END;

(* expecting name of form  file:///home/user/test/src/Test.m3 *)
(* expecting text to be m3 program *)
PROCEDURE Init(self : WorkerClosure) : WorkerClosure =
  VAR
    toolRet : INTEGER;
  BEGIN
    Debug.Write("AstCompile Init\n");

    self.feTool := M3CFETool.GetTool();
    self.warnTool := M3CWarnTool.GetTool();
    self.pathTool := M3PathTool.GetTool();

    (* redirect the m3tk error stream to null to avoid loading the
       output window in vscode *)
    self.errWr := NEW(NullWr.T).init();
    EVAL M3ErrorStream.Set(self.errWr);

    Debug.Write("ToolFrame Startup\n");
    toolRet := M3ToolFrame.Startup(self, compile := FALSE);

    (* setting the -wa arg *)
    (* Not using all warning tools since implicit narrow check
       is producing odd warnings.
    M3Args.SetFlag(self.warnTool, M3CWarnTool.WA_Arg, TRUE);
    *)

    M3Args.SetFlag(self.warnTool, M3CWarnTool.WE_Arg, TRUE);
    M3Args.SetFlag(self.warnTool, M3CWarnTool.WU_Arg, TRUE);
    M3Args.SetFlag(self.warnTool, M3CWarnTool.WR_Arg, TRUE);
    M3Args.SetFlag(self.warnTool, M3CWarnTool.WIF_Arg, TRUE);
    M3Args.SetFlag(self.warnTool, M3CWarnTool.WO_Arg, TRUE);

    RETURN self;
  END Init;

PROCEDURE Compile(self : WorkerClosure; uri, text : TEXT) : RefSeq.T =
  VAR
    stream    : Rd.T;
    gs        : StreamerClosure;
    compileResult: INTEGER;
  BEGIN
    self.uri := uri;
    self.text := text;
    self.getDirs(uri);
    self.setup();
    M3ToolFrame.ResetPath(self.context);

    stream := TextRd.New(self.text);
    gs := NEW(StreamerClosure, name := self.uri, stream := stream);
    M3CGoList.SetStreamer(gs);

    compileResult := M3CFETool.CompileInContext(self.context);
    self.errSeq := M3Error.GetErrSeq();
    RETURN self.errSeq;
  END Compile;

PROCEDURE GetContext(self : WorkerClosure) : M3Context.T =
  BEGIN
    RETURN self.context;
  END GetContext;

PROCEDURE FileName(self : WorkerClosure) : TEXT =
  BEGIN
    RETURN self.uri;
  END FileName;

PROCEDURE Path(self : WorkerClosure; uri : TEXT) : TEXT =
  BEGIN
    self.getDirs(uri);
    RETURN self.prefix;
  END Path;

PROCEDURE GetUnit(self : WorkerClosure; uri : TEXT) : TEXT =
  BEGIN
    self.getDirs(uri);
    RETURN self.unit;
  END GetUnit;

PROCEDURE New() : WorkerClosure =
  BEGIN
    RETURN NEW(WorkerClosure).init();
  END New;

PROCEDURE Setup(self : WorkerClosure) =
  VAR
    dirSize : CARDINAL;
    unitList, pathList, tList  : RAT;
  BEGIN
    Debug.Write("AstCompile Setup\n");

    (* setting the -m or -i Unit arg *)
    unitList := NEW(RAT, 1);
    unitList[0] := self.unit;
    IF self.compType = CompType.Module OR
       self.compType = CompType.GenMod THEN
      M3Args.SetStringList(self.feTool, M3CFETool.Modules_Arg, unitList);
      Debug.Write("Compiling a module\n");
    ELSIF self.compType = CompType.Interface OR
          self.compType = CompType.GenInt THEN
      M3Args.SetStringList(self.feTool, M3CFETool.Interfaces_Arg, unitList);
      Debug.Write("Compiling an interface\n");
    END;
    (* how to handle generics ? can only compile the instantiation *)

    dirSize := self.dirs.size();
    pathList := NEW(RAT, dirSize);
    FOR i := 0 TO dirSize -1 DO
      pathList[i] := self.dirs.get(i);
    END;
    (* setting -D pathnames arg *)
    M3Args.SetPrefix(self.pathTool, M3PathTool.Define_Arg, pathList);

    tList := NEW(RAT, 1);
    tList[0] := self.targetDir & "/.M3IMPTAB";
    (* setting -T.M3IMPTAB arg *)
    M3Args.SetPrefix(self.pathTool, M3PathTool.TFile_Arg, tList);
  END Setup;

(* Not using the worker closure to compile our programs. Just save the
   context for future compiles. *)
PROCEDURE Work(self : WorkerClosure;
               c  : M3Context.T;
               <* UNUSED *> compileResult: INTEGER) : INTEGER RAISES{} =
  VAR
    ret : INTEGER := 0;
  BEGIN
    (* save the context *)
    self.context := c;
    RETURN ret;
  END Work;

(* callback *)
PROCEDURE GetStream(cl: StreamerClosure;
                    cu: M3AST_AS.Compilation_Unit;
                    VAR s: Rd.T) =
  VAR fqn: TEXT;
  BEGIN
    (* need to create fqn from the uid to compare to the cl.name *)
    fqn := M3CUnit.TextName(cu.fe_uid);
    IF Text.Equal(fqn, cl.name) THEN
      s := cl.stream;
    END;
  END GetStream;

PROCEDURE AddSubDirs(p : TEXT; seq : TextSeq.T) =
  VAR
    fs := FS.Iterate(p);
    name,join : TEXT;
    stat : File.Status;
  BEGIN
    WHILE fs.nextWithStatus(name,stat) DO
      IF stat.type = FS.DirectoryFileType THEN
        (* add this name to seq *)
        join := Pathname.Join(p,name);
        seq.addhi(join);
        AddSubDirs(join,seq);
      END;
    END;
    fs.close();
  END AddSubDirs;

PROCEDURE GetDirs(self : WorkerClosure; name : TEXT) =
  VAR
    file,base,posSrc,ext,srcDir : TEXT;
    foundSrc,foundRoot : BOOLEAN;
    TargetDir : TEXT;
  BEGIN
    self.dirs := NEW(TextSeq.T).init();
    TargetDir := MxConfig.HOST();
    (* remove the file:// *)
    (* uri gets passed to the streamer object to compare to the currently
       compiled file *)
    self.uri := Text.Sub(name,7);
    self.prefix := Pathname.Prefix(self.uri);
    file := Pathname.Last(self.uri);
    self.unit := Pathname.Base(file);

    Debug.Write("module:" & file & "\n");
    Debug.Write("unit:" & self.unit & "\n");
    Debug.Write("path:" & self.prefix & "\n");

    posSrc := self.prefix;
    REPEAT
      srcDir := posSrc;
      foundSrc := Text.Equal(Pathname.Last(posSrc),"src");
      foundRoot := Text.Equal(posSrc,"/");
      posSrc := Pathname.Prefix(posSrc);
    UNTIL foundSrc OR foundRoot;

    (* go up dir tree until you find src then add ../target
       and all subdirs of src *)
    IF foundSrc THEN
      Debug.Write("src dir:" & srcDir & "\n");
      base := Pathname.Prefix(srcDir);

      self.targetDir := Pathname.Join(base,TargetDir);
      Debug.Write("target dir:" & self.targetDir & "\n");

      (* add srcDir and target_dir now recurse to find sub dirs *)
       self.dirs.addhi(self.targetDir);
       self.dirs.addhi(srcDir);
       AddSubDirs(srcDir,self.dirs);
    ELSE
     (* what to do? this is case in the test suite and the target is created
        in the same dir. *)
      Debug.Write("Error found root:\n");
    END;

    ext :=  Pathname.LastExt(file);
    IF Text.Equal(ext,"i3") THEN
      self.compType := CompType.Interface;
    ELSIF Text.Equal(ext,"m3") THEN
      self.compType := CompType.Module;
    ELSIF Text.Equal(ext,"ig") THEN
      self.compType := CompType.GenInt;
    ELSIF Text.Equal(ext,"mg") THEN
      self.compType := CompType.GenMod;
    ELSE
      Debug.Write("Error - file error extension " & ext & "\n");
    END;
  END GetDirs;

BEGIN
  (* have to init the warn tool to get warnings *)
  M3CWarnTool.Init();
  (* create the singleton compiler tool object *)
  wc := New();
END AstCompile.
