(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibAnim;
IMPORT Text, ObLib, ObValue, ObEval, SynWr, SynLocation, Point,
       Thread, NetObj, RefList, R2, PaintOp, VBT, GraphVBT,
       GraphVBTExtras, Animate, Trestle, TrestleComm, ObLibUI, Color,
       PaintOpAnim, Rect, RectsVBT, SharedObj; 

  VAR setupDone := FALSE;

  PROCEDURE PackageSetup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      Setup();
    END;
  END PackageSetup;

  PROCEDURE Setup() =
  BEGIN
    SetupRects();
    SetupGraph();
    SetupZeus();
  END Setup;

(* ============ "rects" package ============ *)

TYPE

  RectsCode = 
    {Error, New, SetN, Exists, Delete, Draw, Erase, SetColor,
    SetPosition, GetPosition, SetWorld, SetMargin, SetMins, SetBg,
    Show, Hide};

  RectsOpCode =  
    ObLib.OpCode OBJECT
        code: RectsCode;
      END;
    
  PackageRects = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalRects;
      END;

  VAR rectsException: ObValue.ValException;

  PROCEDURE NewRectsOC(name: TEXT; arity: INTEGER; code: RectsCode)
    : RectsOpCode =
  BEGIN
    RETURN NEW(RectsOpCode, name:=name, arity:=arity, code:=code);
  END NewRectsOC;

  PROCEDURE SetupRects() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RectsCode));
    opCodes^ :=
      OpCodes{
      NewRectsOC("failure", -1, RectsCode.Error),
      NewRectsOC("new", 0, RectsCode.New),
      NewRectsOC("setWorld", 5, RectsCode.SetWorld),
      NewRectsOC("setMargin", 5, RectsCode.SetMargin),
      NewRectsOC("setMins", 3, RectsCode.SetMins),
      NewRectsOC("setBg", 2, RectsCode.SetBg),
      NewRectsOC("setN", 3, RectsCode.SetN),
      NewRectsOC("draw", 2, RectsCode.Draw),
      NewRectsOC("erase", 2, RectsCode.Erase),
      NewRectsOC("exists", 2, RectsCode.Exists),
      NewRectsOC("delete", 3, RectsCode.Delete),
      NewRectsOC("setColor", 4, RectsCode.SetColor),
      NewRectsOC("setPosition", 7, RectsCode.SetPosition),
      NewRectsOC("getPosition", 2, RectsCode.GetPosition),
      NewRectsOC("show", 1, RectsCode.Show),
      NewRectsOC("hide", 1, RectsCode.Hide)};
    ObLib.Register(
      NEW(PackageRects, name := "rects", opCodes:=opCodes));
    rectsException := NEW(ObValue.ValException, name:="rects_failure");
    ObValue.InhibitTransmission(TYPECODE(ValRects), 
      "rects cannot be transmitted/duplicated");
  END SetupRects;

  PROCEDURE EvalRects(self: PackageRects; opCode: ObLib.OpCode; 
                      <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                      <*UNUSED*>temp: BOOLEAN; <*UNUSED*> swr: SynWr.T; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR rs1: ValRects; int1: INTEGER; bool1: BOOLEAN; clr1: Color.T;
      r1: Rect.T; p1,p2: RectsVBT.RealPoint;
      real1, real2, real3, real4: LONGREAL; ar1: REF ARRAY OF ObValue.Val;
    BEGIN
      TRY
      CASE NARROW(opCode, RectsOpCode).code OF
      | RectsCode.Error => 
          RETURN rectsException;
      | RectsCode.SetWorld => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValReal(node) => real1:=node.real;
          ELSE ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValReal(node) => real2:=node.real;
          ELSE ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[4] OF | ObValue.ValReal(node) => real3:=node.real;
          ELSE ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[5] OF | ObValue.ValReal(node) => real4:=node.real;
          ELSE ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          RectsVBT.SetWC(rs1.vbt, 
            FLOAT(real1, REAL), FLOAT(real4, REAL), 
            FLOAT(real2, REAL), FLOAT(real3, REAL));
          RETURN ObValue.valOk;   
      | RectsCode.SetMargin => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValReal(node) => real1:=node.real;
          ELSE ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValReal(node) => real2:=node.real;
          ELSE ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[4] OF | ObValue.ValReal(node) => real3:=node.real;
          ELSE ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[5] OF | ObValue.ValReal(node) => real4:=node.real;
          ELSE ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          RectsVBT.SetMargin(rs1.vbt, 
            FLOAT(real1, REAL), FLOAT(real4, REAL), 
            FLOAT(real2, REAL), FLOAT(real3, REAL));
          RETURN ObValue.valOk;   
      | RectsCode.SetMins => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValReal(node) => real1:=node.real;
          ELSE ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValReal(node) => real2:=node.real;
          ELSE ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          RectsVBT.SetMins(rs1.vbt, FLOAT(real1, REAL), FLOAT(real2, REAL));
          RETURN ObValue.valOk;   
      | RectsCode.SetBg => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObLibUI.ValColor(node) => clr1:=node.color;
          ELSE ObValue.BadArgType(2, "color", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          RectsVBT.SetBg(rs1.vbt, PaintOp.FromRGB(clr1.r, clr1.g, clr1.b,
              mode:=PaintOp.Mode.Accurate));
          RETURN ObValue.valOk;   
      | RectsCode.New => 
          RETURN NEW(ValRects, what:="<a RectsVBT.T>", tag := "RectsVBT`T",
                     picklable:=FALSE, vbt:=NEW(RectsVBT.T).init(),
                     n:=-1, shown:=FALSE); 
      | RectsCode.SetN => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValBool(node) => bool1:=node.bool;
          ELSE ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF int1<0 THEN
            ObValue.BadArgVal(2, "non-negative", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RectsVBT.SetN(rs1.vbt, int1, bool1);
          rs1.n := int1;
          RETURN ObValue.valOk;
      | RectsCode.Exists => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF (int1<0) OR (int1>rs1.n) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RETURN NEW(ObValue.ValBool, bool:=RectsVBT.Exists(rs1.vbt, int1));
      | RectsCode.Delete => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValBool(node) => bool1:=node.bool;
          ELSE ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF int1<0 THEN
            ObValue.BadArgVal(2, "non-negative", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RectsVBT.Delete(rs1.vbt, int1, bool1);
          RETURN ObValue.valOk;
      | RectsCode.SetColor => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObLibUI.ValColor(node) => clr1:=node.color;
          ELSE ObValue.BadArgType(3, "color", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[4] OF | ObValue.ValBool(node) => bool1:=node.bool;
          ELSE ObValue.BadArgType(4, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF (int1<0) OR (int1>rs1.n) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RectsVBT.Color(rs1.vbt, int1, 
            PaintOp.FromRGB(clr1.r, clr1.g, clr1.b,
              mode:=PaintOp.Mode.Accurate), 
            bool1);
          RETURN ObValue.valOk;
      | RectsCode.GetPosition => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF (int1<0) OR (int1>rs1.n) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          r1 := RectsVBT.Locate(rs1.vbt, int1);
          p1 := RectsVBT.VBT2WC(rs1.vbt, 
                  Point.T{h:=r1.west, v:=r1.north});
          p2 := RectsVBT.VBT2WC(rs1.vbt, 
                  Point.T{h:=r1.east, v:=r1.south});
          ar1 := NEW(REF ARRAY OF ObValue.Val, 4);
          ar1^[0] := NEW(ObValue.ValReal, real:=FLOAT(p1.h,LONGREAL), temp:=FALSE);
          ar1^[1] := NEW(ObValue.ValReal, real:=FLOAT(p2.h,LONGREAL), temp:=FALSE);
          ar1^[2] := NEW(ObValue.ValReal, real:=FLOAT(p1.v,LONGREAL), temp:=FALSE);
          ar1^[3] := NEW(ObValue.ValReal, real:=FLOAT(p2.v,LONGREAL), temp:=FALSE);
          RETURN ObValue.NewArray(ar1^);
      | RectsCode.SetPosition => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValReal(node) => real1:=node.real;
          ELSE ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[4] OF | ObValue.ValReal(node) => real2:=node.real;
          ELSE ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[5] OF | ObValue.ValReal(node) => real3:=node.real;
          ELSE ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[6] OF | ObValue.ValReal(node) => real4:=node.real;
          ELSE ObValue.BadArgType(6, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[7] OF | ObValue.ValBool(node) => bool1:=node.bool;
          ELSE ObValue.BadArgType(4, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF (int1<0) OR (int1>rs1.n) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RectsVBT.Position(rs1.vbt, int1,
            FLOAT(real1, REAL), FLOAT(real4, REAL), 
            FLOAT(real2, REAL), FLOAT(real3, REAL),
            bool1);
          RETURN ObValue.valOk;
      | RectsCode.Draw => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF (int1<0) OR (int1>rs1.n) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RectsVBT.Draw(rs1.vbt, int1);
          RETURN ObValue.valOk;
      | RectsCode.Erase => 
          TYPECASE args[1] OF | ValRects(node) => rs1:=node;
          ELSE ObValue.BadArgType(1,"rects",self.name,opCode.name,loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          IF (int1<0) OR (int1>rs1.n) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);<*ASSERT FALSE*> 
          END;
          RectsVBT.Erase(rs1.vbt, int1);
          RETURN ObValue.valOk;
      | RectsCode.Show => 
          TYPECASE args[1] OF 
          | ValRects(node) => 
            IF node.shown THEN
              ObValue.BadArgVal(1, "not already shown", 
                self.name, opCode.name, loc);<*ASSERT FALSE*> 
            END;
            node.shown := TRUE;
            Trestle.Install(node.vbt);
            NARROW(node.vbt,RectsVBT.T).redisplay();
          ELSE ObValue.BadArgType(1, "rects", self.name, opCode.name, loc); <*ASSERT FALSE*> 
          END;
          RETURN ObValue.valOk;      
      | RectsCode.Hide => 
          TYPECASE args[1] OF 
          | ValRects(node) =>
            IF node.shown THEN 
              node.shown := FALSE;
              Trestle.Delete(node.vbt);
            END;
          ELSE ObValue.BadArgType(1, "rects", self.name, opCode.name, loc); <*ASSERT FALSE*> 
          END;
          RETURN ObValue.valOk;      
      END;
      EXCEPT
      | TrestleComm.Failure => 
        ObValue.RaiseException(rectsException, opCode.name, loc);
        <*ASSERT FALSE*> 
      END;
    END EvalRects;


(* ============ "graph" package ============ *)

TYPE

  GraphCode =
    {Error, New, Redisplay, Animate, Clear, SetWorld, SetMargin, SetAspect,
     SetPreferredSize, SetPixelSizeDivisor, VerticesAt, VertexHiLisAt,
     EdgesAt, PolygonsAt, SetClickAction, SetClickReleaseAction,
     SetDoubleClickAction, SetObjectLayer, NewVertex, MoveVertex,
     MoveVertexOnPath, RemoveVertex, VertexToFront, VertexToBack,
     VertexSetSize, VertexSetShape, VertexSetColor, VertexSetFont,
     VertexSetLabel, VertexSetLabelColor, VertexSetBorder,
     VertexSetBorderColor, VertexGetPosition, NewVertexHiLi,
     MoveVertexHiLi, RemoveVertexHiLi, VertexHiLiToFront, VertexHiLiToBack,
     VertexHiLiSetBorder, VertexHiLiSetColor, VertexHiLiGetVertex, NewEdge,
     MoveEdge, MoveEdgeBezier, RemoveEdge, EdgeToFront, EdgeToBack,
     EdgeSetWidth, EdgeSetColor, EdgeSetArrow, EdgeGetVertices,
     EdgeGetControls, NewPolygon, MovePolygon, RemovePolygon,
     PolygonToFront, PolygonToBack, PolygonSetColor, NewFont, DefaultFont,
     NewSpectrum, SetSpectrumColor, SetSpectrumRange, Show, Hide};

  GraphOpCode = ObLib.OpCode OBJECT code: GraphCode;  END;

  PackageGraph = ObLib.T OBJECT OVERRIDES Eval := EvalGraph; END;

PROCEDURE IsVertex (self: ValVertex; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValVertex (oth) => RETURN self.vertex = oth.vertex;
    ELSE
      RETURN FALSE
    END;
  END IsVertex;

PROCEDURE IsVertexHiLi (self: ValVertexHiLi; other: ObValue.ValAnything):
  BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValVertexHiLi (oth) => RETURN self.vertexHiLi = oth.vertexHiLi;
    ELSE
      RETURN FALSE
    END;
  END IsVertexHiLi;

PROCEDURE IsEdge (self: ValEdge; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValEdge (oth) => RETURN self.edge = oth.edge;
    ELSE
      RETURN FALSE
    END;
  END IsEdge;

PROCEDURE IsPolygon (self: ValPolygon; other: ObValue.ValAnything):
  BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValPolygon (oth) => RETURN self.polygon = oth.polygon;
    ELSE
      RETURN FALSE
    END;
  END IsPolygon;

PROCEDURE IsFont (self: ValFont; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValFont (oth) => RETURN self.font = oth.font;
    ELSE
      RETURN FALSE
    END;
  END IsFont;

PROCEDURE IsSpectrum (self: ValSpectrum; other: ObValue.ValAnything):
  BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValSpectrum (oth) => RETURN self.spectrum = oth.spectrum;
    ELSE
      RETURN FALSE
    END;
  END IsSpectrum;

VAR graphException: ObValue.ValException;

PROCEDURE NewGraphOC (name: TEXT; arity: INTEGER; code: GraphCode):
  GraphOpCode =
  BEGIN
    RETURN NEW(GraphOpCode, name := name, arity := arity, code := code);
  END NewGraphOC;

PROCEDURE SetupGraph () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(GraphCode));
    opCodes^ :=
      OpCodes{
        NewGraphOC("failure", -1, GraphCode.Error),
        NewGraphOC("new", 0, GraphCode.New),
        NewGraphOC("redisplay", 1, GraphCode.Redisplay),
        NewGraphOC("animate", 3, GraphCode.Animate),
        NewGraphOC("clear", 1, GraphCode.Clear),
        NewGraphOC("setWorld", 5, GraphCode.SetWorld),
        NewGraphOC("setMargin", 2, GraphCode.SetMargin),
        NewGraphOC("setAspect", 2, GraphCode.SetAspect),
        NewGraphOC("setPreferredSize", 3, GraphCode.SetPreferredSize),
        NewGraphOC("setPixelSizeDivisor", 3, GraphCode.SetPixelSizeDivisor),
        NewGraphOC("verticesAt", 5, GraphCode.VerticesAt),
        NewGraphOC("vertexHiLisAt", 5, GraphCode.VertexHiLisAt),
        NewGraphOC("edgesAt", 5, GraphCode.EdgesAt),
        NewGraphOC("polygonsAt", 5, GraphCode.PolygonsAt),
        NewGraphOC("setClickAction", 2, GraphCode.SetClickAction),
        NewGraphOC(
          "setClickReleaseAction", 2, GraphCode.SetClickReleaseAction),
        NewGraphOC(
          "setDoubleClickAction", 2, GraphCode.SetDoubleClickAction),
        NewGraphOC("setObjectLayer", 2, GraphCode.SetObjectLayer),

        NewGraphOC("newVertex", 1, GraphCode.NewVertex),
        NewGraphOC("moveVertex", 4, GraphCode.MoveVertex),
        NewGraphOC("moveVertexOnPath", 2, GraphCode.MoveVertexOnPath),
        NewGraphOC("removeVertex", 1, GraphCode.RemoveVertex),
        NewGraphOC("vertexToFront", 1, GraphCode.VertexToFront),
        NewGraphOC("vertexToBack", 1, GraphCode.VertexToBack),
        NewGraphOC("setVertexSize", 3, GraphCode.VertexSetSize),
        NewGraphOC("setVertexShape", 2, GraphCode.VertexSetShape),
        NewGraphOC("setVertexColor", 2, GraphCode.VertexSetColor),
        NewGraphOC("setVertexFont", 2, GraphCode.VertexSetFont),
        NewGraphOC("setVertexLabel", 2, GraphCode.VertexSetLabel),
        NewGraphOC("setVertexLabelColor", 2, GraphCode.VertexSetLabelColor),
        NewGraphOC("setVertexBorder", 2, GraphCode.VertexSetBorder),
        NewGraphOC(
          "setVertexBorderColor", 2, GraphCode.VertexSetBorderColor),
        NewGraphOC("getVertexPosition", 1, GraphCode.VertexGetPosition),

        NewGraphOC("newVertexHiLi", 1, GraphCode.NewVertexHiLi),
        NewGraphOC("moveVertexHiLi", 3, GraphCode.MoveVertexHiLi),
        NewGraphOC("removeVertexHiLi", 1, GraphCode.RemoveVertexHiLi),
        NewGraphOC("vertexHiLiToFront", 1, GraphCode.VertexHiLiToFront),
        NewGraphOC("vertexHiLiToBack", 1, GraphCode.VertexHiLiToBack),
        NewGraphOC("setVertexHiLiColor", 2, GraphCode.VertexHiLiSetColor),
        NewGraphOC("setVertexHiLiBorder", 3, GraphCode.VertexHiLiSetBorder),
        NewGraphOC("getVertexHiLiVertex", 1, GraphCode.VertexHiLiGetVertex),

        NewGraphOC("newEdge", 2, GraphCode.NewEdge),
        NewGraphOC("moveEdge", 4, GraphCode.MoveEdge),
        NewGraphOC("moveEdgeBezier", 6, GraphCode.MoveEdgeBezier),
        NewGraphOC("removeEdge", 1, GraphCode.RemoveEdge),
        NewGraphOC("edgeToFront", 1, GraphCode.EdgeToFront),
        NewGraphOC("edgeToBack", 1, GraphCode.EdgeToBack),
        NewGraphOC("setEdgeWidth", 2, GraphCode.EdgeSetWidth),
        NewGraphOC("setEdgeColor", 2, GraphCode.EdgeSetColor),
        NewGraphOC("setEdgeArrows", 3, GraphCode.EdgeSetArrow),
        NewGraphOC("getEdgeVertices", 1, GraphCode.EdgeGetVertices),
        NewGraphOC("getEdgeControls", 1, GraphCode.EdgeGetControls),

        NewGraphOC("newPolygon", 1, GraphCode.NewPolygon),
        NewGraphOC("movePolygon", 3, GraphCode.MovePolygon),
        NewGraphOC("removePolygon", 1, GraphCode.RemovePolygon),
        NewGraphOC("polygonToFront", 1, GraphCode.PolygonToFront),
        NewGraphOC("polygonToBack", 1, GraphCode.PolygonToBack),
        NewGraphOC("setPolygonColor", 2, GraphCode.PolygonSetColor),

        NewGraphOC("newFont", 6, GraphCode.NewFont),
        NewGraphOC("defaultFont", -1, GraphCode.DefaultFont),

        NewGraphOC("newSpectrum", 1, GraphCode.NewSpectrum),
        NewGraphOC("setSpectrumColor", 2, GraphCode.SetSpectrumColor),
        NewGraphOC("setSpectrumRange", 2, GraphCode.SetSpectrumRange),

        NewGraphOC("show", 1, GraphCode.Show),
        NewGraphOC("hide", 1, GraphCode.Hide)};
    ObLib.Register(NEW(PackageGraph, name := "graph", opCodes := opCodes));
    graphException := NEW(ObValue.ValException, name := "graph_failure");
    ObValue.InhibitTransmission(
      TYPECODE(ValGraph), "graphs cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValVertex), "vetices cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValVertexHiLi),
      "vertex hilights cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValEdge), "edges cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValPolygon), "polygons cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValFont), "fonts cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValSpectrum), "spectrums cannot be transmitted/duplicated");
  END SetupGraph;

PROCEDURE EvalGraph (                    self  : PackageGraph;
                                         opCode: ObLib.OpCode;
                     <*UNUSED*>          arity : ObLib.OpArity;
                                READONLY args  : ObValue.ArgArray;
                     <*UNUSED*>          temp  : BOOLEAN;
                     swr: SynWr.T;
                                         loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    gr1                       : Graph;
    gr0                       : ValGraph;
    v1, v2, v3, v4            : GraphVBT.Vertex;
    e1                        : GraphVBT.Edge;
    p1                        : GraphVBT.Polygon;
    real1, real2, real3, real4: LONGREAL;
    list                      : RefList.T;
    size                      : INTEGER;
    bool1, bool2              : BOOLEAN;
    text1, text2, text3, text4: TEXT;
    vh1                       : GraphVBT.VertexHighlight;
    font1                     : GraphVBT.WorldFont;
    fun1                      : ObValue.Val;
    int1, int2                : INTEGER;
    sp1                       : ValSpectrum;
    moveClosure               : MoveClosure;
    cl1                       : ObLibUI.ValColor;
    array1, ar1               : REF ARRAY OF ObValue.Val;
    rl1                       : RefList.T;
  BEGIN
    TRY
      CASE NARROW(opCode, GraphOpCode).code OF
      | GraphCode.Error => RETURN graphException;
      | GraphCode.New =>
          gr1 := NEW(Graph, swr := swr, clickAction := NIL, clickReleaseAction := NIL,
                     doubleClickAction := NIL).init();
          gr0 :=
            NEW(ValGraph, what := "<a GraphVBT.T>", picklable := FALSE,
                tag := "GraphVBT`T", shown := FALSE);
          gr1.valGraph := gr0;
          gr0.vbt := gr1;
          RETURN gr0;
      | GraphCode.Redisplay =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.redisplay();
          RETURN ObValue.valOk;
      | GraphCode.Animate =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Animate.SetDuration(1.0);
          Animate.ResetATime();
          gr1.animate(FLOAT(real1), FLOAT(real2));
          RETURN ObValue.valOk;
      | GraphCode.Clear =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.clear();
          RETURN ObValue.valOk;
      | GraphCode.SetWorld =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValReal (node) => real3 := node.real;
          ELSE
            ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValReal (node) => real4 := node.real;
          ELSE
            ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.setWorld(
            GraphVBT.WorldRectangle{w := FLOAT(real1), e := FLOAT(real2),
                                    n := FLOAT(real3), s := FLOAT(real4)});
          RETURN ObValue.valOk;
      | GraphCode.SetMargin =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.setMargin(FLOAT(real1));
          RETURN ObValue.valOk;
      | GraphCode.SetAspect =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.setAspect(FLOAT(real1));
          RETURN ObValue.valOk;
      | GraphCode.SetPreferredSize =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.setPreferredSize(R2.T{FLOAT(real1), FLOAT(real2)});
          RETURN ObValue.valOk;
      | GraphCode.SetPixelSizeDivisor =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int2 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.setPixelSizeDivisor(
            ARRAY [0 .. 1] OF CARDINAL{MAX(1, int1), MAX(1, int2)});
          RETURN ObValue.valOk;
      | GraphCode.VerticesAt =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValReal (node) => real3 := node.real;
          ELSE
            ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValReal (node) => real4 := node.real;
          ELSE
            ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          rl1 := gr1.verticesAt(
                   WorldRectToScreenRect(gr1.world, VBT.Domain(gr1), real1,
                                         real2, real3, real4));
          int1 := RefList.Length(rl1);
          ar1 := NEW(REF ARRAY OF ObValue.Val, int1);
          FOR i := 0 TO int1 - 1 DO
            ar1^[i] := NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                           tag := "GraphVBT`Vertex", picklable := FALSE,
                           vertex := rl1.head);
            rl1 := rl1.tail;
          END;
          RETURN ObValue.NewArray(ar1^);
      | GraphCode.VertexHiLisAt =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValReal (node) => real3 := node.real;
          ELSE
            ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValReal (node) => real4 := node.real;
          ELSE
            ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          rl1 := gr1.vertexHighlightsAt(
                   WorldRectToScreenRect(gr1.world, VBT.Domain(gr1), real1,
                                         real2, real3, real4));
          int1 := RefList.Length(rl1);
          ar1 := NEW(REF ARRAY OF ObValue.Val, int1);
          FOR i := 0 TO int1 - 1 DO
            ar1^[i] :=
              NEW(ValVertexHiLi, what := "<a GraphVBT.VertexHighlight>",
                  tag := "GraphVBT`VertexHighlight", picklable := FALSE,
                  vertexHiLi := rl1.head);
            rl1 := rl1.tail;
          END;
          RETURN ObValue.NewArray(ar1^);
      | GraphCode.EdgesAt =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValReal (node) => real3 := node.real;
          ELSE
            ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValReal (node) => real4 := node.real;
          ELSE
            ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          rl1 :=
            gr1.edgesAt(WorldRectToScreenRect(gr1.world, VBT.Domain(gr1),
                                              real1, real2, real3, real4));
          int1 := RefList.Length(rl1);
          ar1 := NEW(REF ARRAY OF ObValue.Val, int1);
          FOR i := 0 TO int1 - 1 DO
            ar1^[i] := NEW(ValEdge, what := "<a GraphVBT.Edge>",
                           tag := "GraphVBT`Edge", picklable := FALSE,
                           edge := rl1.head);
            rl1 := rl1.tail;
          END;
          RETURN ObValue.NewArray(ar1^);
      | GraphCode.PolygonsAt =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValReal (node) => real3 := node.real;
          ELSE
            ObValue.BadArgType(4, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValReal (node) => real4 := node.real;
          ELSE
            ObValue.BadArgType(5, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          rl1 := gr1.polygonsAt(
                   WorldRectToScreenRect(gr1.world, VBT.Domain(gr1), real1,
                                         real2, real3, real4));
          int1 := RefList.Length(rl1);
          ar1 := NEW(REF ARRAY OF ObValue.Val, int1);
          FOR i := 0 TO int1 - 1 DO
            ar1^[i] := NEW(ValPolygon, what := "<a GraphVBT.Polygon>",
                           tag := "GraphVBT`Polygon", picklable := FALSE,
                           polygon := rl1.head);
            rl1 := rl1.tail;
          END;
          RETURN ObValue.NewArray(ar1^);
      | GraphCode.SetClickAction =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValFun (node) => fun1 := node;
          ELSE
            ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc);
          END;
          gr1.clickAction := fun1;
          RETURN ObValue.valOk;
      | GraphCode.SetClickReleaseAction =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValFun (node) => fun1 := node;
          ELSE
            ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc);
          END;
          gr1.clickReleaseAction := fun1;
          RETURN ObValue.valOk;
      | GraphCode.SetDoubleClickAction =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValFun (node) => fun1 := node;
          ELSE
            ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          gr1.doubleClickAction := fun1;
          RETURN ObValue.valOk;
      | GraphCode.SetObjectLayer =>
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[1] OF
          | ValVertex (node) =>
              node.vertex.toFront(VAL(int1, GraphVBT.ZOrder));
          | ValVertexHiLi (node) =>
              node.vertexHiLi.toFront(VAL(int1, GraphVBT.ZOrder));
          | ValEdge (node) =>
              node.edge.toFront(VAL(int1, GraphVBT.ZOrder));
          | ValPolygon (node) =>
              node.polygon.toFront(VAL(int1, GraphVBT.ZOrder));
          ELSE
            ObValue.BadArgType(
              1, "graph object", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;

      | GraphCode.NewVertex =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1 := NEW(GraphVBT.Vertex, graph := gr1).init();
          RETURN NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                     tag := "GraphVBT`Vertex", picklable := FALSE,
                     vertex := v1);
      | GraphCode.MoveVertex =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(4, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.move(R2.T{FLOAT(real1), FLOAT(real2)}, bool1, 0.0, 1.0, NIL);
          RETURN ObValue.valOk;
      | GraphCode.MoveVertexOnPath =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValFun (node) => fun1 := node;
          ELSE
            ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          moveClosure := NEW(MoveClosure, swr := swr, 
                             fun := fun1, location := loc);
          (* -- Sets the final vertex position by calling the obliq
             procedure at time 1.0. *)
          v1.move(moveClosure.pos(1.0), TRUE, 0.0, 1.0, moveClosure);
          RETURN ObValue.valOk;
      | GraphCode.RemoveVertex =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.remove();
          RETURN ObValue.valOk;
      | GraphCode.VertexToFront =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.toFront();
          RETURN ObValue.valOk;
      | GraphCode.VertexToBack =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.toBack();
          RETURN ObValue.valOk;
      | GraphCode.VertexSetSize =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.setSize(R2.T{FLOAT(real1), FLOAT(real2)});
          RETURN ObValue.valOk;
      | GraphCode.VertexSetShape =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Equal(text1, "rectangle") THEN
            v1.setShape(GraphVBT.VertexShape.Rectangle);
          ELSIF Text.Equal(text1, "ellipse") THEN
            v1.setShape(GraphVBT.VertexShape.Ellipse);
          ELSE
            ObValue.BadArgVal(2, "\"rectangle\" or \"ellipse\"", self.name,
                              opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;
      | GraphCode.VertexSetColor =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.setColor(
            ExtractColor(args[2], 2, self.name, opCode.name, loc));
          RETURN ObValue.valOk;
      | GraphCode.VertexSetFont =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ValFont (node) => font1 := node.font;
          ELSE
            ObValue.BadArgType(2, "font", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.setFont(font1);
          RETURN ObValue.valOk;
      | GraphCode.VertexSetLabel =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.setLabel(text1);
          RETURN ObValue.valOk;
      | GraphCode.VertexSetLabelColor =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.setFontColor(
            ExtractColor(args[2], 2, self.name, opCode.name, loc));
          RETURN ObValue.valOk;
      | GraphCode.VertexSetBorder =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          v1.setBorder(FLOAT(real1));
          RETURN ObValue.valOk;
      | GraphCode.VertexSetBorderColor =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          (* -- v1.setBorderColor(ExtractColor(args[2], 2, self.name,
             opCode.name, loc)); *)
          v1.setFontColor(
            ExtractColor(args[2], 2, self.name, opCode.name, loc));
          RETURN ObValue.valOk;
      | GraphCode.VertexGetPosition =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          ar1 := NEW(REF ARRAY OF ObValue.Val, 2);
          ar1^[0] :=
            NEW(ObValue.ValReal, real := FLOAT(v1.pos[0], LONGREAL),
                temp := FALSE);
          ar1^[1] :=
            NEW(ObValue.ValReal, real := FLOAT(v1.pos[1], LONGREAL),
                temp := FALSE);
          RETURN ObValue.NewArray(ar1^);

      | GraphCode.NewVertexHiLi =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1 := NEW(GraphVBT.VertexHighlight, vertex := v1).init();
          RETURN NEW(ValVertexHiLi, what := "<a GraphVBT.VertexHighlight>",
                     tag := "GraphVBT`VertexHighlight", picklable := FALSE,
                     vertexHiLi := vh1);
      | GraphCode.MoveVertexHiLi =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(2, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1.move(v1, bool1);
          RETURN ObValue.valOk;
      | GraphCode.RemoveVertexHiLi =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1.remove();
          RETURN ObValue.valOk;
      | GraphCode.VertexHiLiToFront =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1.toFront();
          RETURN ObValue.valOk;
      | GraphCode.VertexHiLiToBack =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1.toBack();
          RETURN ObValue.valOk;
      | GraphCode.VertexHiLiSetColor =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1.setColor(
            ExtractColor(args[2], 2, self.name, opCode.name, loc));
          RETURN ObValue.valOk;
      | GraphCode.VertexHiLiSetBorder =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real2 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vh1.setBorder(R2.T{FLOAT(real1), FLOAT(real2)});
          RETURN ObValue.valOk;
      | GraphCode.VertexHiLiGetVertex =>
          TYPECASE args[1] OF
          | ValVertexHiLi (node) => vh1 := node.vertexHiLi;
          ELSE
            ObValue.BadArgType(
              1, "vertexHiLi", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                     tag := "GraphVBT`Vertex", picklable := FALSE,
                     vertex := vh1.vertex);

      | GraphCode.NewEdge =>
          TYPECASE args[1] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(1, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ValVertex (node) => v2 := node.vertex;
          ELSE
            ObValue.BadArgType(2, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1 := NEW(GraphVBT.Edge, vertex0 := v1, vertex1 := v2).init();
          RETURN
            NEW(ValEdge, what := "<a GraphVBT.Edge>",
                tag := "GraphVBT`Edge", picklable := FALSE, edge := e1);
      | GraphCode.MoveEdge =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(2, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ValVertex (node) => v2 := node.vertex;
          ELSE
            ObValue.BadArgType(3, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(4, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.move(v1, v2, NIL, NIL, bool1);
          RETURN ObValue.valOk;
      | GraphCode.MoveEdgeBezier =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ValVertex (node) => v1 := node.vertex;
          ELSE
            ObValue.BadArgType(2, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ValVertex (node) => v2 := node.vertex;
          ELSE
            ObValue.BadArgType(3, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ValVertex (node) => v3 := node.vertex;
          ELSE
            ObValue.BadArgType(4, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ValVertex (node) => v4 := node.vertex;
          ELSE
            ObValue.BadArgType(5, "vertex", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[6] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(6, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.move(v1, v2, v3, v4, bool1);
          RETURN ObValue.valOk;
      | GraphCode.RemoveEdge =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.remove();
          RETURN ObValue.valOk;
      | GraphCode.EdgeToFront =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.toFront();
          RETURN ObValue.valOk;
      | GraphCode.EdgeToBack =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.toBack();
          RETURN ObValue.valOk;
      | GraphCode.EdgeSetWidth =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.setWidth(FLOAT(real1));
          RETURN ObValue.valOk;
      | GraphCode.EdgeSetColor =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.setColor(
            ExtractColor(args[2], 2, self.name, opCode.name, loc));
          RETURN ObValue.valOk;
      | GraphCode.EdgeSetArrow =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(2, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (node) => bool2 := node.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          e1.setArrow(ARRAY [0 .. 1] OF BOOLEAN{bool1, bool2});
          RETURN ObValue.valOk;
      | GraphCode.EdgeGetVertices =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          ar1 := NEW(REF ARRAY OF ObValue.Val, 2);
          ar1^[0] := NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                         tag := "GraphVBT`Vertex", picklable := FALSE,
                         vertex := e1.vertex0);
          ar1^[1] := NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                         tag := "GraphVBT`Vertex", picklable := FALSE,
                         vertex := e1.vertex1);
          RETURN ObValue.NewArray(ar1^);
      | GraphCode.EdgeGetControls =>
          TYPECASE args[1] OF
          | ValEdge (node) => e1 := node.edge;
          ELSE
            ObValue.BadArgType(1, "edge", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF (e1.control0 = NIL) OR (e1.control1 = NIL) THEN
            ar1 := NEW(REF ARRAY OF ObValue.Val, 0);
            RETURN ObValue.NewArray(ar1^);
          ELSE
            ar1 := NEW(REF ARRAY OF ObValue.Val, 2);
            ar1^[0] := NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                           tag := "GraphVBT`Vertex", picklable := FALSE,
                           vertex := e1.control0);
            ar1^[1] := NEW(ValVertex, what := "<a GraphVBT.Vertex>",
                           tag := "GraphVBT`Vertex", picklable := FALSE,
                           vertex := e1.control1);
            RETURN ObValue.NewArray(ar1^);
          END;

      | GraphCode.NewPolygon =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          size := NUMBER(array1^);
          list := NIL;
          FOR i := 0 TO size - 1 DO
            TYPECASE array1^[(size - 1) - i] OF
            | ValVertex (node) => list := RefList.Cons(node.vertex, list);
            ELSE
              ObValue.BadArgType(
                1, "array(vertex)", self.name, opCode.name, loc); <*ASSERT FALSE*>
            END;
          END;
          p1 := NEW(GraphVBT.Polygon, vertices := list).init();
          RETURN NEW(ValPolygon, what := "<a GraphVBT.Polygon>",
                     tag := "GraphVBT`Polygon", picklable := FALSE,
                     polygon := p1);
      | GraphCode.MovePolygon =>
          TYPECASE args[1] OF
          | ValPolygon (node) => p1 := node.polygon;
          ELSE
            ObValue.BadArgType(1, "polygon", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          size := NUMBER(array1^);
          list := NIL;
          FOR i := 0 TO size - 1 DO
            TYPECASE array1^[(size - 1) - i] OF
            | ValVertex (node) => list := RefList.Cons(node.vertex, list);
            ELSE
              ObValue.BadArgType(
                1, "array(vertex)", self.name, opCode.name, loc); <*ASSERT FALSE*>
            END;
          END;
          p1.move(list, bool1);
          RETURN ObValue.valOk;
      | GraphCode.RemovePolygon =>
          TYPECASE args[1] OF
          | ValPolygon (node) => p1 := node.polygon;
          ELSE
            ObValue.BadArgType(1, "polygon", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p1.remove();
          RETURN ObValue.valOk;
      | GraphCode.PolygonToFront =>
          TYPECASE args[1] OF
          | ValPolygon (node) => p1 := node.polygon;
          ELSE
            ObValue.BadArgType(1, "polygon", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p1.toFront();
          RETURN ObValue.valOk;
      | GraphCode.PolygonToBack =>
          TYPECASE args[1] OF
          | ValPolygon (node) => p1 := node.polygon;
          ELSE
            ObValue.BadArgType(1, "polygon", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p1.toBack();
          RETURN ObValue.valOk;
      | GraphCode.PolygonSetColor =>
          TYPECASE args[1] OF
          | ValPolygon (node) => p1 := node.polygon;
          ELSE
            ObValue.BadArgType(1, "polygon", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p1.setColor(
            ExtractColor(args[2], 2, self.name, opCode.name, loc));
          RETURN ObValue.valOk;

      | GraphCode.NewFont =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValText (node) => text3 := node.text;
          ELSE
            ObValue.BadArgType(5, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[6] OF
          | ObValue.ValText (node) => text4 := node.text;
          ELSE
            ObValue.BadArgType(6, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          font1 := gr1.font(text1, FLOAT(real1), ExtractSlant(text2),
                            text3, text4);
          RETURN NEW(ValFont, what := "<a GraphVBT.WorldFont>",
                     tag := "GraphVBT`WorldFont", picklable := FALSE,
                     font := font1);
      | GraphCode.DefaultFont =>
          RETURN NEW(ValFont, what := "<a GraphVBT.WorldFont>",
                     tag := "GraphVBT`WorldFont", picklable := FALSE,
                     font := GraphVBT.DefaultFont);

      | GraphCode.NewSpectrum =>
          TYPECASE args[1] OF
          | ValGraph (node) => gr1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ValSpectrum, what := "<a GraphVBT.Spectrum>",
                     tag := "GraphVBT`Spectrum", picklable := FALSE,
                     graph := gr1,
                     spectrum := NEW(PaintOpAnim.T).init(Color.Black));
      | GraphCode.SetSpectrumColor =>
          TYPECASE args[1] OF
          | ValSpectrum (node) => sp1 := node;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObLibUI.ValColor (node) => cl1 := node;
          ELSE
            ObValue.BadArgType(2, "color", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          sp1.spectrum.set(sp1.graph, cl1.color);
          RETURN ObValue.valOk;
      | GraphCode.SetSpectrumRange =>
          TYPECASE args[1] OF
          | ValSpectrum (node) => sp1 := node;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValFun (node) => fun1 := node;
          ELSE
            ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          sp1.spectrum.animate(
            sp1.graph, NEW(SpectrumClosure, swr := swr, fun := fun1, location := loc));
          RETURN ObValue.valOk;

      | GraphCode.Show =>
          TYPECASE args[1] OF
          | ValGraph (node) =>
              IF node.shown THEN
                ObValue.BadArgVal(
                  1, "not already shown", self.name, opCode.name, loc); <*ASSERT FALSE*>
              END;
              node.shown := TRUE;
              Trestle.Install(node.vbt);
              NARROW(node.vbt, GraphVBT.T).redisplay();
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;
      | GraphCode.Hide =>
          TYPECASE args[1] OF
          | ValGraph (node) =>
              IF node.shown THEN
                node.shown := FALSE;
                Trestle.Delete(node.vbt);
              END;
          ELSE
            ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | TrestleComm.Failure =>
        ObValue.RaiseException(graphException, opCode.name, loc); <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc); <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          self.name & "_" & opCode.name, atoms, loc); <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalGraph;

TYPE
  SpectrumClosure = PaintOpAnim.Animation OBJECT
                      fun     : ObValue.ValFun;
                      location: SynLocation.T;
                      swr     : SynWr.T;
                    OVERRIDES
                      rgb := SpectrumRangeClosure;
                    END;

PROCEDURE SpectrumRangeClosure (self: SpectrumClosure; t: REAL): Color.T
  RAISES {} =
  (* Can't produce any good error messages because it must raise {} *)
  VAR
    v   : ObValue.Val;
    args: ARRAY [0 .. 0] OF ObValue.Val;
  BEGIN
    TRY
      args[0] :=
        NEW(ObValue.ValReal, real := FLOAT(t, LONGREAL), temp := FALSE);
      v := ObEval.Call(self.fun, args, self.swr, self.location);
      TYPECASE v OF
      | ObLibUI.ValColor (node) => RETURN node.color;
      | ValSpectrum (node) => RETURN node.spectrum.get();
      ELSE
        ObValue.RaiseError(
          "argument of graph_setSpectrumRange must return a color",
          self.location);        <*ASSERT FALSE*>
      END;
    EXCEPT
    | ObValue.Error (packet) =>
        SynWr.Text(
          self.swr,
          "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
        ObValue.ErrorMsg(self.swr, packet);
        SynWr.Flush(self.swr);
        RETURN Color.Black;
    | ObValue.Exception (packet) =>
        SynWr.Text(
          self.swr,
          "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
        ObValue.ExceptionMsg(self.swr, packet);
        SynWr.Flush(self.swr);
        RETURN Color.Black;
    END;
  END SpectrumRangeClosure;

TYPE
  MoveClosure = GraphVBT.AnimationPath OBJECT
                  fun     : ObValue.ValFun;
                  location: SynLocation.T;
                  swr     : SynWr.T;
                OVERRIDES
                  pos := MoveOnPathClosure;
                END;

PROCEDURE MoveOnPathClosure (self: MoveClosure; t: REAL): R2.T RAISES {} =
  (* Can't produce any good error messages because it must raise {} *)
  VAR
    v, vx, vy: ObValue.Val;
    rx, ry   : REAL;
    args     : ARRAY [0 .. 0] OF ObValue.Val;
  BEGIN
    TRY
      args[0] :=
        NEW(ObValue.ValReal, real := FLOAT(t, LONGREAL), temp := FALSE);
      v := ObEval.Call(self.fun, args, self.swr, self.location);
      TYPECASE v OF
      | ObValue.ValArray (node) =>
          TRY
            vx := node.Get(0);
            vy := node.Get(1);
          EXCEPT
          | ObValue.ServerError (msg) =>
              ObValue.RaiseError(msg, self.location);
          | NetObj.Error (atoms) =>
              ObValue.RaiseNetException(
                "on remote array access", atoms, self.location);
          | SharedObj.Error (atoms) =>
              ObValue.RaiseSharedException(
                "on replicated array access", atoms, self.location);
          | Thread.Alerted =>
              ObValue.RaiseException(
                ObValue.threadAlerted, "on remote array access",
                self.location);
          END;
      ELSE
        ObValue.RaiseError(
          "argument of graph_moveOnPath must return an array(2,real)",
          self.location);
      END;
      TYPECASE vx OF
      | ObValue.ValReal (node) => rx := FLOAT(node.real, REAL);
      ELSE
        ObValue.RaiseError(
          "argument of graph_moveOnPath must return an array(2,real)",
          self.location);
      END;
      TYPECASE vy OF
      | ObValue.ValReal (node) => ry := FLOAT(node.real, REAL);
      ELSE
        ObValue.RaiseError(
          "argument of graph_moveOnPath must return an array(2,real)",
          self.location);
      END;
      RETURN R2.T{rx, ry};
    EXCEPT
    | ObValue.Error (packet) =>
        SynWr.Text(self.swr, 
          "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
        ObValue.ErrorMsg(self.swr, packet);
        SynWr.Flush(self.swr);
        RETURN R2.T{0.0, 0.0};
    | ObValue.Exception (packet) =>
        SynWr.Text(self.swr,
          "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
        ObValue.ExceptionMsg(self.swr, packet);
        SynWr.Flush(self.swr);
        RETURN R2.T{0.0, 0.0};
    END;
  END MoveOnPathClosure;

PROCEDURE ExtractColor (ob          : ObValue.Val;
                        argNo       : INTEGER;
                        name, opName: TEXT;
                        loc         : SynLocation.T): PaintOp.T
  RAISES {ObValue.Error} =
  BEGIN
    TYPECASE ob OF
    | ObLibUI.ValColor (node) =>
        RETURN PaintOp.FromRGB(node.color.r, node.color.g, node.color.b,
                               mode := PaintOp.Mode.Accurate);
    | ValSpectrum (node) => RETURN node.spectrum.op();
    ELSE
      ObValue.BadArgType(argNo, "color or spectrum", name, opName, loc); <*ASSERT FALSE*>
    END;
  END ExtractColor;

PROCEDURE ExtractSlant (slant: TEXT): GraphVBT.Slant =
  BEGIN
    IF Text.Equal(slant, "Roman") THEN
      RETURN GraphVBT.Slant.Roman;
    ELSIF Text.Equal(slant, "Italic") THEN
      RETURN GraphVBT.Slant.Italic;
    ELSIF Text.Equal(slant, "Oblique") THEN
      RETURN GraphVBT.Slant.Oblique;
    ELSIF Text.Equal(slant, "ReverseItalic") THEN
      RETURN GraphVBT.Slant.ReverseItalic;
    ELSIF Text.Equal(slant, "ReverseOblique") THEN
      RETURN GraphVBT.Slant.ReverseOblique;
    ELSIF Text.Equal(slant, "Other") THEN
      RETURN GraphVBT.Slant.Other;
    ELSIF Text.Equal(slant, "Any") THEN
      RETURN GraphVBT.Slant.Any;
    ELSE
      RETURN GraphVBT.Slant.Roman;
    END;
  END ExtractSlant;

PROCEDURE Mouse (self: Graph; READONLY cd: VBT.MouseRec) =
  VAR
    r2  : R2.T;
    args: ARRAY [0 .. 2] OF ObValue.Val;
  BEGIN
    TRY
      IF (cd.clickType = VBT.ClickType.FirstDown) AND (cd.clickCount = 0) THEN
        IF self.clickAction = NIL THEN RETURN END;
        r2 := GraphVBTExtras.ScreenPtToWorldPos(self, cd.cp.pt);
        args[0] := self.valGraph;
        args[1] := NEW(ObValue.ValReal, real := FLOAT(r2[0], LONGREAL),
                       temp := FALSE);
        args[2] := NEW(ObValue.ValReal, real := FLOAT(r2[1], LONGREAL),
                       temp := FALSE);
        EVAL ObEval.Call(
               self.clickAction, args, self.swr, self.clickAction.fun.location);
      ELSIF (cd.clickType = VBT.ClickType.LastUp) AND (cd.clickCount <= 1) THEN
        IF self.clickReleaseAction = NIL THEN RETURN END;
        r2 := GraphVBTExtras.ScreenPtToWorldPos(self, cd.cp.pt);
        args[0] := self.valGraph;
        args[1] := NEW(ObValue.ValReal, real := FLOAT(r2[0], LONGREAL),
                       temp := FALSE);
        args[2] := NEW(ObValue.ValReal, real := FLOAT(r2[1], LONGREAL),
                       temp := FALSE);
        EVAL ObEval.Call(self.clickReleaseAction, args, self.swr,
                         self.clickReleaseAction.fun.location);
      ELSIF (cd.clickType = VBT.ClickType.FirstDown)
              AND (cd.clickCount = 2) THEN
        IF self.doubleClickAction = NIL THEN RETURN END;
        r2 := GraphVBTExtras.ScreenPtToWorldPos(self, cd.cp.pt);
        args[0] := self.valGraph;
        args[1] := NEW(ObValue.ValReal, real := FLOAT(r2[0], LONGREAL),
                       temp := FALSE);
        args[2] := NEW(ObValue.ValReal, real := FLOAT(r2[1], LONGREAL),
                       temp := FALSE);
        EVAL ObEval.Call(self.doubleClickAction, args, self.swr,
                         self.doubleClickAction.fun.location);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
        SynWr.Text(
          self.swr,
          "*** a graph_ click action caused an Obliq error: ***\n");
        ObValue.ErrorMsg(self.swr, packet);
        SynWr.Flush(self.swr);
    | ObValue.Exception (packet) =>
        SynWr.Text(
          self.swr,
          "*** a graph_ click action caused an Obliq exception: ***\n");
        ObValue.ExceptionMsg(self.swr, packet);
        SynWr.Flush(self.swr);
    END;
  END Mouse;

PROCEDURE WorldRectToScreenRect (world     : GraphVBT.WorldRectangle;
                                 domain    : Rect.T;
                                 w, e, n, s: LONGREAL                 ):
  Rect.T =
  VAR
    domainWidth, domainHeight, worldWidth, worldHeight: REAL;
    r                                                 : Rect.T;
  BEGIN
    domainWidth := FLOAT(domain.east) - FLOAT(domain.west);
    domainHeight := FLOAT(domain.south) - FLOAT(domain.north);
    worldWidth := world.e - world.w;
    worldHeight := world.s - world.n;
    IF (worldWidth = 0.0) OR (worldHeight = 0.0) THEN
      RETURN Rect.Empty
    END;
    r := Rect.T{
           west := domain.west + ROUND((FLOAT(w) - world.w) * domainWidth
                                         / worldWidth), east :=
           domain.west
             + ROUND((FLOAT(e) - world.w) * domainWidth / worldWidth),
           north := domain.north + ROUND((FLOAT(n) - world.n)
                                           * domainHeight / worldHeight),
           south := domain.north + ROUND((FLOAT(s) - world.n)
                                           * domainHeight / worldHeight)};
    IF r.east = r.west THEN r.east := r.east + 1 END;
    IF r.north = r.south THEN r.south := r.south + 1 END;
    RETURN r;
  END WorldRectToScreenRect;

(* ============ "zeus" package ============ *)

TYPE

  ZeusCode = 
    {Error, Animate};
    
  ZeusOpCode =  
    ObLib.OpCode OBJECT
        code: ZeusCode;
      END;

  PackageZeus = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalZeus;
      END;

  VAR zeusException: ObValue.ValException;

  PROCEDURE SetupZeus() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ZeusCode));
    opCodes^ :=
      OpCodes{
      NEW(ZeusOpCode, name:="failure", arity:=-1, code:=ZeusCode.Error),
      NEW(ZeusOpCode, name:="animate", 
          arity:=3, code:=ZeusCode.Animate)
      };
    ObLib.Register(
      NEW(PackageZeus, name:="zeus", opCodes:=opCodes));
    zeusException := NEW(ObValue.ValException, name:="zeus_failure");
  END SetupZeus;

  PROCEDURE EvalZeus(self: PackageZeus; opCode: ObLib.OpCode; 
                     <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                     <*UNUSED*>temp: BOOLEAN; <*UNUSED*> swr: SynWr.T;
                     loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR gr1: Graph; real1, real2: LONGREAL; 
    BEGIN
      TRY
      CASE NARROW(opCode, ZeusOpCode).code OF
      | ZeusCode.Error => 
          RETURN graphException;
      | ZeusCode.Animate => 
          TYPECASE args[1] OF | ValGraph(node) => gr1:=node.vbt;
          ELSE ObValue.BadArgType(1, "graph", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValReal(node) => real1:=node.real;
          ELSE ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[3] OF | ObValue.ValReal(node) => real2:=node.real;
          ELSE ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          gr1.animate(FLOAT(real1), FLOAT(real2));
          RETURN ObValue.valOk;
      END;
      EXCEPT
      | Thread.Alerted =>
          ObValue.RaiseException(ObValue.threadAlerted, 
                               self.name&"_"&opCode.name,loc);<*ASSERT FALSE*> 
      END;
    END EvalZeus;

BEGIN
END ObLibAnim.
