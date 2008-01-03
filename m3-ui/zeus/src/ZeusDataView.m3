(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  6 16:12:23 PDT 1993 by mhb      *)
(*      modified on Fri Jul 31  3:58:00 PDT 1992 by sclafani *)
(*      modified on Tue Jul 28 23:37:26 PDT 1992 by johnh *)

MODULE ZeusDataView;
<* PRAGMA LL *>

IMPORT Algorithm, Axis, DataView, Filter, FormsVBT, Rd, Rsrc, TextVBT, Thread,
       VBT, View, Zeus, ZeusClass;

<* FATAL Rd.Failure, Thread.Alerted, FormsVBT.Error *>

REVEAL
  T = View.T BRANDED OBJECT
        tc: CARDINAL;
        slate: TextVBT.T;
      OVERRIDES
        isCompat := IsCompat;
        startrun := Startrun;
        shape := Shape;
      END;

PROCEDURE IsCompat (<* UNUSED *> v: T; alg: ZeusClass.T):
  BOOLEAN =
  <* LL = arbitrary *>
  BEGIN
    RETURN NARROW(alg, Algorithm.T).varRsrc # NIL
  END IsCompat;

PROCEDURE Startrun (view: T) =
  <* LL.sup < VBT.mu *>
  <* FATAL Rsrc.NotFound *>
  VAR
    alg        := Zeus.Resolve(view).alg;
    ch : VBT.T;
  BEGIN
    LOCK VBT.mu DO
      (* This code is serialized among all data views.  Just
         before Zeus calls Startrun, it NILs alg.varView.  Thus,
         only the first data view whose Startrun executes will
         actually install the varRsrc form. *)
      IF alg.varRsrc = NIL THEN 
        (*
        TextVBT.Put(view.slate,
          "No variable-view form specified in this algorithm.");
        ch := view.slate;
        *)
        <* ASSERT FALSE *>
      ELSIF alg.varView # NIL THEN
        TextVBT.Put(
          view.slate,
          "Another variable-view installed.");
        ch := view.slate;
      ELSE
        alg.varView :=
          NEW(DataView.T).initFromRsrc(alg.varRsrc, alg.varPath);
        ch := alg.varView;
      END;
      IF ch # Filter.Child(view) THEN
        EVAL Filter.Replace(view, ch)
      END
    END;
    View.T.startrun(view);
  END Startrun;

CONST
  DefaultShape = 
     VBT.SizeRange{lo   := VBT.DefaultShape.lo, 
                   pref := 300,
                   hi   := VBT.DefaultShape.hi};

PROCEDURE Shape (<* UNUSED *> v   : VBT.T;
                 <* UNUSED *> axis: Axis.T;
                 <* UNUSED *> n   : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN DefaultShape
  END Shape;

PROCEDURE New (): View.T =
  <* LL.sup <= VBT.mu *>
  VAR ch := TextVBT.New ("");
  BEGIN
    RETURN NEW (T, slate := ch).init (ch);
  END New;

BEGIN
END ZeusDataView.
