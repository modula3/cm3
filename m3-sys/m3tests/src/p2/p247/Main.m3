(*
Reduced from 3-ui/anim3D/src/x-opengl/X_OpenGL_Base.m3
SPARC64_SOLARIS
../src/x-opengl/X_OpenGL_Base.m3: In function 'X_OpenGL_Base__xSetDepthcueing':
../src/x-opengl/X_OpenGL_Base.m3:886:0: internal compiler error: in function_arg_record_value, at config/sparc/sparc.c:5198
Please submit a full bug report,
with preprocessed source if appropriate.
*)
UNSAFE MODULE Main;
IMPORT Color;
<*NOWARN*>IMPORT RegExpParse;
IMPORT RT0, RTLink, RTIO;
TYPE ColorT = RECORD a,b,c: REAL; END;

TYPE O1 = OBJECT END;
TYPE O2 = OBJECT a:=0 END;

<*NOWARN*> VAR o1:O1;
<*NOWARN*> VAR o2:O2;

<*NOWARN*> PROCEDURE F1 (a,b,c,d,e,f:INTEGER; g:ColorT) =
BEGIN
  a := b;
  e := ROUND(g.a);
END F1;

<*NOWARN*> PROCEDURE F2 (a,b,c,d,e,f:INTEGER; g:Color.T) =
BEGIN
  b := c;
  d := TRUNC(g.b);
END F2;

VAR binder_called := FALSE;

PROCEDURE Binder(<*NOWARN*> mode: INTEGER): RT0.ModulePtr =
BEGIN
  RTIO.PutText("in Binder\n");
  RTIO.Flush();
  binder_called := TRUE;
  RETURN NIL;
END Binder;

VAR imp: RT0.ImportInfo;
VAR mod: RT0.ModuleInfo;

BEGIN
  F1(1,2,3,4,5,6,ColorT{1.0,2.0,3.0});
  F2(2,3,4,5,6,7,Color.T{2.0,3.0,4.0});
  imp.binder := Binder;
  mod.imports := ADR(imp);
  RTLink.FixImports(ADR(mod));
  <* ASSERT binder_called *>
END Main.
