GENERIC MODULE CommandLoop(Context);
FROM CommandLoop IMPORT Error;
IMPORT Pathname;
IMPORT CommandLoop;
IMPORT FmtScanVar;
IMPORT ScalarVar;
IMPORT Fmt;
IMPORT TextList;
IMPORT Term;

(* "CommandLoop"s for the Layman *)

REVEAL
  T = Public BRANDED OBJECT
    ctx  : Context.T;
    cl   : CommandLoop.T;
  OVERRIDES
    init       := Init;
    c          := PutCommand;
    integer    := Integer;
    longReal   := LongReal;
    boolean    := Boolean;
    run        := Run;
    setPreStep := SetPreStep;
    setPostStep:= SetPostStep;
  END;


PROCEDURE Init(self: T; ctx: Context.T; prompt := "> "): T =
  BEGIN
    self.cl := NEW(CommandLoop.T).init(prompt);
    self.ctx := ctx;
    RETURN self;
  END Init;

PROCEDURE PutCommand(self: T; cmd: Command; names: TEXT;
                     simpleHelp, extendedHelp: TEXT := NIL) =

  BEGIN
    self.cl.putCommand(names, NEW(CommandObject,
                                  cmd:=cmd,
                                  simpleHelp:=simpleHelp,
                                  extHelp := extendedHelp,
                                  hasExtendedHelp := (extendedHelp # NIL),
                                  ctx := self.ctx));
  END PutCommand;

TYPE
  CommandObject = CommandLoop.Command OBJECT
    ctx: Context.T;
    cmd: Command;
    extHelp: TEXT;
  OVERRIDES
    execute := Execute;
    extendedHelp := ExtendedHelp;
  END;

PROCEDURE Execute(co: CommandObject;
                  args: TextList.T;
                  term: Term.T)
  RAISES {Error} =
  BEGIN
    IF co.cmd # NIL THEN
      co.cmd(co.ctx, args, term);
    END;
  END Execute;

PROCEDURE ExtendedHelp(co: CommandObject;
                       <*UNUSED*>args: TextList.T): TEXT RAISES {Error} =
  BEGIN
    IF co.extHelp = NIL THEN
      RAISE Error("No extended help provided for this command.");
    END;
    RETURN co.extHelp;
  END ExtendedHelp;

PROCEDURE Run(self: T; sourcePath: Pathname.T := NIL) =
  BEGIN
    self.cl.run(sourcePath);
  END Run;

PROCEDURE SetPreStep(self: T; cmd: Command := NIL) =
  BEGIN
    IF cmd = NIL THEN
      self.cl.setPreStep(NIL);
    ELSE
      self.cl.setPreStep(NEW(CommandObject, cmd:=cmd, ctx:=self.ctx));
    END;
  END SetPreStep;

PROCEDURE SetPostStep(self: T; cmd: Command := NIL) =
  BEGIN
    IF cmd = NIL THEN
      self.cl.setPostStep(NIL);
    ELSE
      self.cl.setPostStep(NEW(CommandObject, cmd:=cmd, ctx:=self.ctx));
    END;
  END SetPostStep;



(*****************************************************************************
 *                                                                           *
 *                        commands to change variables                       *
 *                                                                           *
 *****************************************************************************)

PROCEDURE Integer(self: T;
                  name: TEXT; desc: TEXT := NIL;
                  default := 0;
                  lo := FIRST(INTEGER);
                  hi := LAST(INTEGER);
                  base := 10;
                  userCanChange := TRUE): REF INTEGER =
  VAR
    res := NEW(REF INTEGER);
    fs := FmtScanVar.Int(res, base, lo, hi);
  BEGIN
    res^ := default;
    ScalarVar.PutCommand(self.cl, fs, "integer", name, desc, userCanChange);
    RETURN res;
  END Integer;
  
PROCEDURE Boolean(self: T; name: TEXT;desc:TEXT:=NIL;
                  default:=FALSE;chg:=TRUE): REF BOOLEAN =
  VAR
    res := NEW(REF BOOLEAN);
    fs := FmtScanVar.Bool(res);
  BEGIN
    res^ := default;
    ScalarVar.PutCommand(self.cl, fs, "boolean", name, desc, chg);
    RETURN res;
  END Boolean;

PROCEDURE LongReal(self: T; name: TEXT;desc:TEXT:=NIL;
                   default:=0.0D0;style:=Fmt.Style.Auto;
                   digits:=10;chg:=TRUE):REF LONGREAL=
  VAR
    res := NEW(REF LONGREAL);
    fs := FmtScanVar.LongReal(res, digits, style);
  BEGIN
    res^ := default;
    ScalarVar.PutCommand(self.cl, fs, "number", name, desc, chg);
    RETURN res;                              
  END LongReal;

BEGIN
END CommandLoop.
