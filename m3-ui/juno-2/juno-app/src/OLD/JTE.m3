(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 12 09:52:09 PDT 1992 by heydon                   *)
(*      modified on Sat Aug 22 23:32:19 1992 by myers                        *)
<* PRAGMA LL *>

MODULE JTE;

IMPORT JunoTextEditor AS TextEditor, JunoClosure AS Closure, JunoBundle;
IMPORT TextEditVBT, TextPort, Shadow, FVTypes;
IMPORT Trestle, TrestleComm, VBT, VBTClass, Axis, Point, Rect, Split, ZSplit;
IMPORT Rd, Wr, FileStream, Text, Thread, Rsrc;

FROM FVTypes IMPORT FVTextEdit;
FROM Debug IMPORT Print;

IMPORT FormsVBT;

FROM Stdio IMPORT stderr;

<* FATAL Thread.Alerted, Rsrc.NotFound *>
<* FATAL TrestleComm.Failure, FormsVBT.Unimplemented, FormsVBT.Error *>

TYPE
  Form = FormsVBT.T OBJECT
    te: TextEditor.T;
    closure: Closure.T;
    load_file, save_file: FormsVBT.T;
  OVERRIDES
    realize := Realize
  END;

  MyTE = FVTextEdit OBJECT OVERRIDES
    init := InitMyTE
  END;

PROCEDURE ApplicationError (msg: TEXT; near: VBT.T) =
  <* LL.sup = VBT.mu *>
  <*FATAL Split.NotAChild*>
  <* FATAL Wr.Failure *>
  BEGIN
    IF near = NIL THEN near := fv.te END;
    IF VBT.Parent(app_error) # NIL THEN
      Split.Delete(VBT.Parent(app_error), app_error)
    END;
    FormsVBT.PutText(app_error, "msg", msg);
    VAR v := near; BEGIN
      WHILE NOT ISTYPE(v, FVTypes.FVZSplit) DO (* or Name(v) # "z" *)
        v := VBT.Parent(v)
      END;
      IF v = NIL THEN
        Wr.PutText(stderr, msg & "\n");
        RETURN
      END;
      VAR st := VBT.ScreenTypeOf(v); BEGIN
        VBTClass.Rescreen(app_error, st);
        VAR
          sh      := VBTClass.GetShapes(app_error);
          neardom := VBT.Domain(near);
          fudge := Point.T{ROUND(10.0 * st.res[Axis.T.Hor]),
                           ROUND(5.0 * st.res[Axis.T.Ver])};
        BEGIN
          ZSplit.InsertAt(
            v, app_error,
            Point.Max(Point.Add(Rect.NorthWest(VBT.Domain(v)), fudge),
                      Point.T{neardom.west - sh[Axis.T.Hor].pref - fudge.h,
                              neardom.north + fudge.v}))
        END
      END
    END
  END ApplicationError;

PROCEDURE DismissError(
    <*UNUSED*> ae: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> cl: REFANY;
    <*UNUSED*> rs: VBT.TimeStamp) =
  <* FATAL Split.NotAChild *>
  BEGIN
    IF VBT.Parent(app_error) # NIL THEN
      Split.Delete(VBT.Parent(app_error), app_error)
    END
  END DismissError;

PROCEDURE MakeButton(
    form: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> cl: REFANY;
    <*UNUSED*> rs: VBT.TimeStamp) =
  VAR errmsg: TEXT; fv: Form := form; BEGIN
    IF NOT TextEditor.Parse(fv.te, errmsg) THEN
      ApplicationError(errmsg, fv)
    ELSIF fv.closure # NIL THEN
      fv.closure.invoke()
    END
  END MakeButton;

PROCEDURE LoadButton(
    <*UNUSED*> form: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> cl: REFANY;
    <*UNUSED*> rs: VBT.TimeStamp) =
  BEGIN
    IF Trestle.ScreenOf(fv.load_file, Point.Origin).trsl = NIL THEN
      Trestle.Attach(fv.load_file);
      Trestle.Decorate(fv.load_file, windowTitle := "Load File");
      Trestle.MoveNear(fv.load_file, fv) (* use of global *)
    END
  END LoadButton;

PROCEDURE SaveButton(
    <*UNUSED*> form: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> cl: REFANY;
    <*UNUSED*> rs: VBT.TimeStamp) =
  BEGIN
    IF Trestle.ScreenOf(fv.save_file, Point.Origin).trsl = NIL THEN
      Trestle.Attach(fv.save_file);
      Trestle.Decorate(fv.save_file, windowTitle := "Save File");
      Trestle.MoveNear(fv.save_file, fv) (* use of global *)
    END
  END SaveButton;

PROCEDURE LoadFile(
    lf: FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> cl: REFANY;
    <*UNUSED*> rs: VBT.TimeStamp) =
  VAR fname := FormsVBT.GetText(lf, "fb"); BEGIN
    TRY
      VAR rd := FileStream.OpenRead(fname); BEGIN
        IF rd = NIL THEN RAISE Rd.Failure(NIL) END;
        TextPort.SetText(fv.te, Rd.GetText(rd, LAST(CARDINAL)));
        FormsVBT.PutText(fv.save_file, "fb", fname)
      END
    EXCEPT
      Rd.Failure => ApplicationError("Cannot read from file: " & fname, fv)
    END;
    Trestle.Delete(lf)
  END LoadFile;

PROCEDURE SaveFile (
    sf   : FormsVBT.T;
    <*UNUSED*> event: TEXT;
    <*UNUSED*> cl   : REFANY;
    <*UNUSED*> rs   : VBT.TimeStamp) =
  VAR fname := FormsVBT.GetText(sf, "fb"); BEGIN
    TRY
      VAR wr := FileStream.OpenWrite(fname); BEGIN
        IF wr = NIL THEN RAISE Wr.Failure(NIL) END;
        Wr.PutText(wr, TextPort.GetText(fv.te));
        Wr.Close(wr)
      END
    EXCEPT
      Wr.Failure => ApplicationError("Cannot write to file: " & fname, fv)
    END;
    Trestle.Delete(sf)
  END SaveFile;

PROCEDURE InitMyTE(
    myte     : MyTE;
    kind     : TextEditVBT.Kind  := TextEditVBT.Kind.Scrollable;
    style    : Shadow.Style      := Shadow.Style.Flat;
    scrollbar: TextPort.Scroller := NIL;
    shadow   : Shadow.T          := NIL;
    expandOnDemand := FALSE): TextEditVBT.T =
  <* LL <= VBT.mu *>
  BEGIN
    myte.port := fv.te;         (* use of global *)
    EVAL FVTextEdit.init(myte, kind, style, scrollbar, shadow, expandOnDemand);
    RETURN myte
  END InitMyTE;

PROCEDURE Realize (fv: Form; typ, name: TEXT): VBT.T =
  BEGIN
    IF Text.Equal(name, "jte")
      THEN RETURN NEW(MyTE)
      ELSE RETURN FormsVBT.T.realize(fv, typ, name)
    END
  END Realize;

VAR
  <* LL >= VBT.mu *>
  app_error: VBT.T;
  fv: Form;

PROCEDURE New(
    file: TEXT;
    VAR (* OUT *) te: TextEditor.T;
    c: Closure.T := NIL): VBT.T =
  VAR src: TEXT; loadedFile := FALSE; BEGIN
    LOCK VBT.mu DO
      fv := NEW(Form, closure := c);
      IF file # NIL THEN
        TRY
          src := Rd.GetText(FileStream.OpenRead(file), LAST(CARDINAL));
          loadedFile := TRUE
        EXCEPT
          Rd.Failure => src := ""
        END
      ELSE
        src := ""
      END;
      fv.te := TextEditor.Init(NEW(TextEditor.T), src);
      te := fv.te;
      TRY
        VAR path := Rsrc.BuildPath("$JunoPATH", JunoBundle.Get()); BEGIN
          fv.load_file := NEW(FormsVBT.T).initFromRsrc("LoadFile.fv", path);
          fv.save_file := NEW(FormsVBT.T).initFromRsrc("SaveFile.fv", path);
          app_error    := NEW(FormsVBT.T).initFromRsrc("AppError.fv", path);
          EVAL fv.initFromRsrc("JTE.fv", path)
        END;
        FormsVBT.AttachProc(fv, "make", MakeButton);
        FormsVBT.AttachProc(fv, "load", LoadButton);
        FormsVBT.AttachProc(fv, "save", SaveButton);
        FormsVBT.AttachProc(fv.load_file, "fb", LoadFile);
        FormsVBT.AttachProc(fv.save_file, "fb", SaveFile);
        FormsVBT.AttachProc(app_error, "dismiss", DismissError);
        IF loadedFile THEN FormsVBT.PutText(fv.save_file, "fb", file) END
      EXCEPT
        Rd.Failure => Print("Could not read file\n")
      | FormsVBT.Error => Print("FormsVBT error\n")
      END
    END;
    RETURN fv
  END New;

BEGIN
END JTE.
