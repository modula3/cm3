(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Wed Jun  8 14:00:31 PDT 1994 by mhb    *)
(*      modified on Thu Jun 24 09:31:22 PDT 1993 by steveg *)
(*      modified on Mon Feb 15 20:03:16 PST 1993 by johnh *)


MODULE Classes;

IMPORT Algorithm, AlgorithmClass, RefList, Text, View, ViewClass, ZeusPanel;

TYPE
  AlgInfo = REF RECORD
                  proc  : ZeusPanel.NewAlgProc;
                  name  : TEXT;
                END;

VAR mu := NEW(MUTEX);    (* protect classes against simultaneous access *)

VAR
  algs: RefList.T (* of AlgInfo *);

PROCEDURE RegisterAlg (proc: ZeusPanel.NewAlgProc; name: TEXT) =
  VAR algInfo := NEW(AlgInfo, name := name, proc := proc);
  BEGIN
    LOCK mu DO algs := RefList.Append(algs, RefList.List1(algInfo)) END;
  END RegisterAlg;

PROCEDURE FindAlg (name: TEXT): INTEGER RAISES {NotFound} =
  VAR
    which := 0;
    rest  := algs;
  BEGIN
    LOCK mu DO
      WHILE rest # NIL DO
        IF Text.Equal(name, NARROW(rest.head, AlgInfo).name) THEN
          RETURN which
        END;
        INC(which);
        rest := rest.tail
      END;
    END;
    RAISE NotFound;
  END FindAlg;

PROCEDURE NewAlg (which: INTEGER): Algorithm.T =
  VAR alg: Algorithm.T;
  BEGIN
    LOCK mu DO
      IF which >= RefList.Length(algs) THEN RETURN NIL END;
      WITH algInfo = NARROW(RefList.Nth(algs, which), AlgInfo) DO
        alg := algInfo.proc();
        alg.name := algInfo.name;
        RETURN alg;
      END
    END;
  END NewAlg;

PROCEDURE AlgCount (): INTEGER =
  BEGIN
    LOCK mu DO RETURN RefList.Length(algs) END
  END AlgCount;


TYPE
  ViewInfo = REF RECORD
                   proc     : ZeusPanel.NewViewProc;
                   name     : TEXT;
                   alertable: BOOLEAN;
                   sample   : View.T;
                 END;

VAR
  views: RefList.T (* of ViewInfo *);

PROCEDURE RegisterView (proc     : ZeusPanel.NewViewProc;
                        name     : TEXT;
                        alertable: BOOLEAN;
                        sample   : View.T                 ) =
  VAR
    viewInfo := NEW(ViewInfo, name := name, alertable := alertable,
                    sample := sample, proc := proc);
  BEGIN
    LOCK mu DO views := RefList.Append(views, RefList.List1(viewInfo)) END
  END RegisterView;

PROCEDURE FindView (name: TEXT): INTEGER RAISES {NotFound} =
  VAR
    which := 0;
    rest  := views;
  BEGIN
    LOCK mu DO
      WHILE rest # NIL DO
        IF Text.Equal(name, NARROW(rest.head, ViewInfo).name) THEN
          RETURN which
        END;
        INC(which);
        rest := rest.tail
      END;
    END;
    RAISE NotFound;
  END FindView;

PROCEDURE NewView (which: INTEGER): View.T =
  VAR view: View.T;
  BEGIN
    LOCK mu DO
      IF which >= RefList.Length(views) THEN RETURN NIL END;
      WITH viewInfo = NARROW(RefList.Nth(views, which), ViewInfo) DO
        view := viewInfo.proc();
        IF view # NIL THEN
          view.name := viewInfo.name;
          view.alertable := viewInfo.alertable
        END;
        RETURN view;
      END
    END;
  END NewView;

PROCEDURE SampleView (which: INTEGER): View.T =
  BEGIN
    LOCK mu DO
      IF which >= RefList.Length(views) THEN RETURN NIL END;
      WITH viewInfo = NARROW(RefList.Nth(views, which), ViewInfo) DO
        IF viewInfo.sample = NIL THEN
          viewInfo.sample := viewInfo.proc();
          viewInfo.sample.name := viewInfo.name;
          viewInfo.sample.alertable := viewInfo.alertable;
        END;
        RETURN viewInfo.sample;
      END
    END;
  END SampleView;

PROCEDURE ViewCount (): INTEGER =
  BEGIN
    LOCK mu DO RETURN RefList.Length(views) END
  END ViewCount;


BEGIN
END Classes.
