(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Feb  9 07:58:36 PST 1995 by kalsow *)
(*      modified on Fri Jan  6 00:31:55 PST 1995 by najork *)
(*      modified on Wed Sep 22 09:33:26 1993 by luca *)
(*      modified on Mon Sep 20 08:40:49 PDT 1993 by hania *)

<* PRAGMA LL *>

MODULE AlgSubtype;

IMPORT Algorithm, FormsVBT, Thread, ZeusPanel, RefList, ZeusCodeView;
IMPORT SubtypeAlgClass, SubtypeIE;
IMPORT Text, TextRd, Sx, Atom, VBT;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE
  T = SubtypeAlgClass.T BRANDED OBJECT
        trail: ARRAY [0..1000] OF RECORD lft,rht: Type END;
        top: INTEGER := -1;
        travTrail: ARRAY [0..1000] OF Type;
        travTop: INTEGER;
        nodes: ARRAY [0..99] OF Type;
        nodeIndex: INTEGER;
        edgeIndex: INTEGER;
      OVERRIDES
        run := Run;
      END;

PROCEDURE New (): Algorithm.T =
  VAR form: FormsVBT.T;
  BEGIN
    form := ZeusPanel.NewForm("subtypeinput.fv");
    FormsVBT.AttachProc(form, "ex1", Example1Proc, NIL);
    FormsVBT.AttachProc(form, "ex2", Example2Proc, NIL);
    FormsVBT.AttachProc(form, "ex3", Example3Proc, NIL);
    FormsVBT.AttachProc(form, "ex4", Example4Proc, NIL);
    FormsVBT.AttachProc(form, "ex5", Example5Proc, NIL);
    FormsVBT.AttachProc(form, "ex6", Example6Proc, NIL);
    FormsVBT.AttachProc(form, "ex7", Example7Proc, NIL);
    FormsVBT.AttachProc(form, "ex8", Example8Proc, NIL);
    FormsVBT.AttachProc(form, "ex9", Example9Proc, NIL);
    FormsVBT.AttachProc(form, "ex10", Example10Proc, NIL);
    
    FormsVBT.AttachProc(form, "nonex1", NonExample1Proc, NIL);
    FormsVBT.AttachProc(form, "nonex2", NonExample2Proc, NIL);
    FormsVBT.AttachProc(form, "nonex3", NonExample3Proc, NIL);
    FormsVBT.AttachProc(form, "nonex4", NonExample4Proc, NIL);
    FormsVBT.AttachProc(form, "nonex5", NonExample5Proc, NIL);
    FormsVBT.AttachProc(form, "nonex6", NonExample6Proc, NIL);
    FormsVBT.AttachProc(form, "nonex7", NonExample7Proc, NIL);
    FormsVBT.AttachProc(form, "nonex8", NonExample8Proc, NIL);
    FormsVBT.AttachProc(form, "nonex9", NonExample9Proc, NIL);
    FormsVBT.AttachProc(form, "nonex10", NonExample10Proc, NIL);
    RETURN 
      NEW(T, data := form,
        codeViews := 
          RefList.List1(RefList.List2("M3 Code View", "alg_m3.code"))).init();
  END New;

PROCEDURE Example1Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(top -> bot)");  
    FormsVBT.PutText(fv, "typeright", "(bot -> top)");   
  END Example1Proc;

PROCEDURE Example2Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "((top -> bot) -> bot)");  
    FormsVBT.PutText(fv, "typeright", "(bot -> (bot -> top))");  
  END Example2Proc;

PROCEDURE Example3Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (top -> x))");  
    FormsVBT.PutText(fv, "typeright", "(rec y (bot -> y))"); 
  END Example3Proc;

PROCEDURE Example4Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (top -> x))");  
    FormsVBT.PutText(fv, "typeright", "(rec y (bot -> (bot -> y)))"); 
  END Example4Proc;

PROCEDURE Example5Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (x -> bot))");  
    FormsVBT.PutText(fv, "typeright", "(rec y (y -> bot))"); 
  END Example5Proc;

PROCEDURE Example6Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x ((x -> top) -> x))");  
    FormsVBT.PutText(fv, "typeright", "(rec y (y -> (bot -> y)))"); 
  END Example6Proc;

PROCEDURE Example7Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (x -> x))");  
    FormsVBT.PutText(fv, "typeright", "(rec y (y -> y))"); 
  END Example7Proc;

PROCEDURE Example8Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (x -> x))");  
    FormsVBT.PutText(fv, "typeright", "(rec y ((y -> y) -> y))"); 
  END Example8Proc;

PROCEDURE Example9Proc(             fv:        FormsVBT.T; 
                       <* UNUSED *> name:      TEXT;
                       <* UNUSED *> eventData: REFANY; 
                       <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (x -> (x -> x)))");  
    FormsVBT.PutText(fv, "typeright", "(rec y ((y -> y) -> y))"); 
  END Example9Proc;

PROCEDURE Example10Proc(             fv:        FormsVBT.T; 
                        <* UNUSED *> name:      TEXT;
                        <* UNUSED *> eventData: REFANY; 
                        <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec t (top -> ((t -> t) -> top)))");  
    FormsVBT.PutText(fv, "typeright", "(rec s (top -> ((s -> bot) -> top)))");  
  END Example10Proc;

PROCEDURE NonExample1Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(bot -> bot)");  
    FormsVBT.PutText(fv, "typeright", "(top -> top)");  
  END NonExample1Proc;

PROCEDURE NonExample2Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (x -> bot))");  
    FormsVBT.PutText(fv, "typeright", "(rec y (y -> top))"); 
  END NonExample2Proc;

PROCEDURE NonExample3Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x (x -> bot))");  
    FormsVBT.PutText(fv, "typeright", "(rec y ((y -> top) -> top))"); 
  END NonExample3Proc;

PROCEDURE NonExample4Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "(rec x ((x -> bot) -> bot))");  
    FormsVBT.PutText(fv, "typeright", "(rec y ((y -> top) -> top))"); 
  END NonExample4Proc;

PROCEDURE NonExample5Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "top");  
    FormsVBT.PutText(fv, "typeright", "bot");  
  END NonExample5Proc;

PROCEDURE NonExample6Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "top");
    FormsVBT.PutText(fv, "typeright", "bot");
  END NonExample6Proc;

PROCEDURE NonExample7Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "top");
    FormsVBT.PutText(fv, "typeright", "bot");
  END NonExample7Proc;

PROCEDURE NonExample8Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "top");
    FormsVBT.PutText(fv, "typeright", "bot");
  END NonExample8Proc;

PROCEDURE NonExample9Proc(             fv:        FormsVBT.T; 
                          <* UNUSED *> name:      TEXT;
                          <* UNUSED *> eventData: REFANY; 
                          <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "top");
    FormsVBT.PutText(fv, "typeright", "bot");
  END NonExample9Proc;

PROCEDURE NonExample10Proc(             fv:        FormsVBT.T; 
                           <* UNUSED *> name:      TEXT;
                           <* UNUSED *> eventData: REFANY; 
                           <* UNUSED *> time:      VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "typeleft", "top");
    FormsVBT.PutText(fv, "typeright", "bot");
  END NonExample10Proc;

(* ======================================================= *)

  TYPE
    Type = OBJECT index: INTEGER END;
    TypeBot = Type BRANDED OBJECT END;
    TypeTop = Type BRANDED OBJECT END;
    TypeFun = 
      Type BRANDED OBJECT 
        dom,rng: Type; 
        domEdgeIndex,rngEdgeIndex: INTEGER;
      END;

  PROCEDURE Notice(alg: T; lft, rht: Type) =
  BEGIN
    INC(alg.top);
    alg.trail[alg.top].lft := lft;
    alg.trail[alg.top].rht := rht;
  END Notice;

  PROCEDURE Seen(alg: T; lft, rht: Type): BOOLEAN =
  BEGIN
    FOR i:=0 TO alg.top DO
      IF (alg.trail[i].lft=lft) AND (alg.trail[i].rht=rht) THEN 
        RETURN TRUE 
      END;
    END;
    RETURN FALSE;
  END Seen;

  PROCEDURE In(alg: T; lft, rht: Type; 
    lftLeadingEdgeIndex, rhtLeadingEdgeIndex: INTEGER): BOOLEAN
    RAISES {Thread.Alerted} =

      PROCEDURE At(line: INTEGER) RAISES {Thread.Alerted} =
        BEGIN ZeusCodeView.At(alg, line) END At;

      VAR lftRes, rhtRes: BOOLEAN; res: BOOLEAN;
      BEGIN
         ZeusCodeView.Enter(alg, procedureName := "In");
         SubtypeIE.Enter(alg, lft.index, rht.index, 
                 lftLeadingEdgeIndex, rhtLeadingEdgeIndex);
At(1);   IF Seen(alg, lft, rht) THEN 
At(12);      SubtypeIE.SeenOK(alg, lft.index, rht.index);
             res :=  TRUE;
         ELSE
At(2);     SubtypeIE.Notice(alg, lft.index, rht.index);
           Notice(alg, lft, rht);
At(3);     TYPECASE lft OF
           | TypeBot => 
At(4);        SubtypeIE.BotLessAnyOK(alg, lft.index, rht.index, 
                 lftLeadingEdgeIndex, rhtLeadingEdgeIndex);
               res :=  TRUE;
           | TypeTop => 
At(5);         IF ISTYPE(rht, TypeTop)
               THEN
                 SubtypeIE.TopLessTopOK(alg, lft.index, rht.index, 
                   lftLeadingEdgeIndex, rhtLeadingEdgeIndex);     
                 res :=  TRUE;
               ELSE
                 SubtypeIE.TopLessNonTopKO(alg, lft.index, rht.index, 
                   lftLeadingEdgeIndex, rhtLeadingEdgeIndex);
                 res :=  FALSE;
               END;
           | TypeFun(lftFun) =>
At(6);         TYPECASE rht OF
               | TypeBot => 
At(7);            SubtypeIE.FunLessBotKO(alg, lft.index, rht.index, 
                     lftLeadingEdgeIndex, rhtLeadingEdgeIndex );
                   res :=  FALSE;
               | TypeTop => 
At(8);             SubtypeIE.FunLessTopOK(alg, lft.index, rht.index, 
                     lftLeadingEdgeIndex, rhtLeadingEdgeIndex);
                   res :=  TRUE;
               | TypeFun(rhtFun) =>
At(10);            SubtypeIE.FunLessFun(alg, lft.index, rht.index, 
                     lftLeadingEdgeIndex, rhtLeadingEdgeIndex);
                   lftRes := In(alg, rhtFun.dom, lftFun.dom,
                               rhtFun.domEdgeIndex, lftFun.domEdgeIndex);
                   IF lftRes THEN
At(11);              rhtRes := In(alg, lftFun.rng, rhtFun.rng,
                               lftFun.rngEdgeIndex, rhtFun.rngEdgeIndex);
                     res := lftRes AND rhtRes;
                   ELSE
                     res := lftRes;
                   END;
               ELSE <* ASSERT FALSE *>
               END;
           ELSE <* ASSERT FALSE *>
           END;
         END;
     SubtypeIE.Exit(alg, lft.index, rht.index, 
       lftLeadingEdgeIndex, rhtLeadingEdgeIndex, res);
     ZeusCodeView.Exit(alg);
     RETURN res;
  END In;

(* ======================================================= *)

  PROCEDURE TravNotice(alg: T; type: Type) =
  BEGIN
    INC(alg.travTop);
    alg.travTrail[alg.travTop] := type;
  END TravNotice;

  PROCEDURE TravSeen(alg: T; type: Type): BOOLEAN =
  BEGIN
    FOR i:=0 TO alg.travTop DO
      IF alg.travTrail[i]=type THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END TravSeen;

PROCEDURE NewBot(): Type =
  BEGIN
    RETURN NEW(TypeBot, index:=-1);
  END NewBot;
  
PROCEDURE NewTop(): Type =
  BEGIN
    RETURN NEW(TypeTop, index:=-1);
  END NewTop;
  
PROCEDURE NewFun(dom, rng: Type): Type =
  BEGIN
    RETURN NEW(TypeFun, index:=-1, dom:=dom, rng:=rng);
  END NewFun;

PROCEDURE Traverse(alg: T; type: Type) RAISES {Thread.Alerted} =
  BEGIN
    alg.travTop := -1;
    Traverse1(alg, type, type);
  END Traverse;

PROCEDURE Traverse1(alg: T; type: Type; parent: Type) RAISES {Thread.Alerted} =
  BEGIN
    IF TravSeen(alg, type) THEN 
      SubtypeIE.NewLoop(alg, parent.index, type.index);
      RETURN 
    END;
    TravNotice(alg, type);
    TYPECASE type OF
    | TypeBot => 
        INC(alg.nodeIndex);
        type.index := alg.nodeIndex;
        alg.nodes[alg.nodeIndex] := type;
        SubtypeIE.NewBot(alg, type.index);
    | TypeTop => 
        INC(alg.nodeIndex);
        type.index := alg.nodeIndex;
        alg.nodes[alg.nodeIndex] := type;
        SubtypeIE.NewTop(alg, type.index);
    | TypeFun(fun) =>
        INC(alg.nodeIndex);
        type.index := alg.nodeIndex;
        alg.nodes[alg.nodeIndex] := type;
        INC(alg.edgeIndex);
        fun.domEdgeIndex := alg.edgeIndex;
        INC(alg.edgeIndex);
        fun.rngEdgeIndex := alg.edgeIndex;
        SubtypeIE.NewFun(alg, type.index, 
          fun.domEdgeIndex, fun.rngEdgeIndex);
        Traverse1(alg, fun.dom, type);
        Traverse1(alg, fun.rng, type);
        SubtypeIE.NewDomRng(alg, type.index, fun.dom.index, fun.rng.index);
    ELSE <* ASSERT FALSE *>
    END;
  END Traverse1;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR lft, rht: Type; lftRoot, rhtRoot: INTEGER;
  BEGIN
    SubtypeIE.Setup(alg);

   TRY
     lft := ParseSx(Sx.Read(TextRd.New(
         FormsVBT.GetText(alg.data, "typeleft"))), NIL);
   EXCEPT ELSE
     lft := NewBot();
   END;
   
   TRY
     rht := ParseSx(Sx.Read(TextRd.New(
         FormsVBT.GetText(alg.data, "typeright"))), NIL);
   EXCEPT ELSE
     rht := NewBot();
   END;
  
    alg.nodeIndex := -1;
    alg.edgeIndex := -1;
    lftRoot := alg.nodeIndex+1;
    Traverse(alg, lft);
    rhtRoot := alg.nodeIndex+1;
    Traverse(alg, rht);
    SubtypeIE.Begin(alg, lftRoot, rhtRoot);
    
    IF In(alg, lft, rht, -1, -1) THEN
      SubtypeIE.OK(alg, lft.index, rht.index, -1, -1);
    ELSE
      SubtypeIE.KO(alg, lft.index, rht.index, -1, -1);
    END;
  END Run;


(* ======================================================= *)

(*
Type ::=
  "bot"
  "top"
  FunType
  "(" "rec" Ide FunType ")"
  
FunType ::=
  "(" Type "->" Type ")"
*)

EXCEPTION ParseSxError;

TYPE SxEnv = OBJECT
    ide: TEXT;
    val: TypeFun;
    rest: SxEnv;
  END;

PROCEDURE NewEnv(ide: TEXT; val: TypeFun; rest: SxEnv): SxEnv =
  BEGIN
    RETURN NEW(SxEnv, ide:=ide, val:=val, rest:=rest);
  END NewEnv;

PROCEDURE Lookup(ide: TEXT; env: SxEnv): TypeFun RAISES {ParseSxError} =
  BEGIN
    IF env=NIL THEN RAISE ParseSxError END;
    IF Text.Equal(ide, env.ide) THEN 
      RETURN env.val;
    ELSE
      RETURN Lookup(ide, env.rest);
    END;
  END Lookup;

PROCEDURE IsFunSx(list: RefList.T): BOOLEAN =
  BEGIN
    IF RefList.Length(list) # 3 THEN RETURN FALSE END;
    TYPECASE RefList.Nth(list, 1) OF
    | Atom.T(atom) => RETURN Text.Equal("->", Atom.ToText(atom));
    ELSE RETURN FALSE;
    END;
  END IsFunSx;

PROCEDURE IsRecSx(list: RefList.T): BOOLEAN =
  BEGIN
    IF RefList.Length(list) # 3 THEN RETURN FALSE END;
    TYPECASE RefList.Nth(list, 0) OF
    | Atom.T(atom) => RETURN Text.Equal("rec", Atom.ToText(atom));
    ELSE RETURN FALSE;
    END;
  END IsRecSx;

PROCEDURE ParseSx(sx: Sx.T; env: SxEnv): Type RAISES {ParseSxError} =
  VAR ide: TEXT; fun: TypeFun;
  BEGIN
    TYPECASE sx OF
    | Atom.T(atom) => 
        ide := Atom.ToText(atom);
        IF Text.Equal(ide, "bot") THEN
          RETURN NewBot();
        ELSIF Text.Equal(ide, "top") THEN
          RETURN NewTop();
        ELSE RETURN Lookup(ide, env);
        END;
    | RefList.T(list) =>
        IF IsFunSx(list) THEN
          RETURN NewFun(
            ParseSx(RefList.Nth(list, 0), env),
            ParseSx(RefList.Nth(list, 2), env));
        ELSIF IsRecSx(list) THEN
          TYPECASE RefList.Nth(list, 1) OF
          | Atom.T(atom) =>
              ide := Atom.ToText(atom);
              fun := NewFun(NIL, NIL);
              TYPECASE ParseSx(RefList.Nth(list, 2), NewEnv(ide, fun, env)) OF
              | TypeFun(fun2) =>
                  fun.dom := fun2.dom;
                  fun.rng := fun2.rng;
                  RETURN fun;
               ELSE RAISE ParseSxError;
               END;
            ELSE RAISE ParseSxError;
            END;
        ELSE RAISE ParseSxError;
        END;
    ELSE RAISE ParseSxError;
    END;
  END ParseSx;

PROCEDURE FmtBool(bool: BOOLEAN): TEXT =
  BEGIN
    IF bool THEN RETURN "true" ELSE RETURN "false" END;
  END FmtBool;

BEGIN
  ZeusPanel.RegisterAlg(New, "Subtyping Recursive Types", "Subtype");
END AlgSubtype.
