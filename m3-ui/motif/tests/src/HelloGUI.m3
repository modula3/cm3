(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 11:27:37 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE HelloGUI;
(*
Abstract: Interface for Hello

11/16/94  Harry George
          Initial version
*)

IMPORT RTLinker, IO, M3toC;
IMPORT X, Xt, XtN, Xm, XmN, Xmw;

FROM Xmacro IMPORT
  XtVaSetValues, XtVaCreateManagedWidget,
  CharVal, IntVal;

FROM M3toC IMPORT
  TtoS;

FROM Ctypes IMPORT
  char_star;

VAR
  display:X.DisplayStar;
  appContext: Xt.AppContext;
  topLevel:Xt.Widget;

(*-----------------*)
(* Utilities       *)
(*-----------------*)
VAR Module:="HelloGUI";

(*-----------------*)
PROCEDURE debug(level:INTEGER; ftn, str:TEXT) =
BEGIN
  IF verbosity >= level THEN
    (*debugging levels*)
    IO.Put (Module & ":" & ftn & ":" & str);
  END;
END debug;

(*=====================*)
(* X and Motif stuff   *)
(*=====================*)
VAR
  mainw,menubar,
  runbutton,textfield:Xt.Widget;
  global_txt:TEXT;

(*-------------------*)
PROCEDURE MakeWidgets(str:TEXT) =
CONST ftn = "MakeWidgets";
VAR
  str1:Xm.String;
BEGIN
  debug(1,ftn,"begin\n");

  (*---save str parm as global var---*)
  global_txt:=str;

  (*---main window---*)
  mainw:=XtVaCreateManagedWidget(TtoS("xtest_main_window"),
        Xmw.xmMainWindowWidgetClass,topLevel,
        XmN.scrollBarDisplayPolicy,IntVal(Xm.AS_NEEDED),
        XmN.scrollingPolicy,IntVal(Xm.AUTOMATIC));

  (*---menubar---*)
  menubar:=Xmw.CreateMenuBar(mainw,
        TtoS("menubar"),NIL,0);

  (*---runbutton---*)
  str1:=Xm.StringCreateSimple(TtoS("Run"));  
  runbutton:=XtVaCreateManagedWidget(TtoS("button1"),
        Xmw.xmPushButtonWidgetClass,menubar,
        XmN.labelString,str1,
        XmN.mnemonic,CharVal('R'));
  Xm.StringFree(str1);
  Xt.AddCallback(runbutton,
                 callback_name:=XmN.activateCallback,
                 callback:=runbutton_cb); 

  (*---manage menubar---*)
  Xt.ManageChild(menubar);

  (*---text field---*)
  textfield:=XtVaCreateManagedWidget(TtoS("field1"),
        Xmw.xmTextFieldWidgetClass,mainw,
        XmN.columns,IntVal(15));
  XtVaSetValues(mainw,
                XmN.workWindow,textfield);  

END MakeWidgets;

(*---------------------*)
PROCEDURE runbutton_cb(<*UNUSED*> w         : Xt.Widget;
                       <*UNUSED*> closure   : Xt.Pointer;
                       <*UNUSED*> call_data : Xt.Pointer) =
(*callback for runbutton*)
CONST ftn = "runbutton_cb";
VAR str:char_star;
BEGIN
  debug(1,ftn,"begin\n");

  
  (*---use Hello's txt as str to put in field---*)
  str:=TtoS(global_txt);
  Xmw.TextFieldSetString(textfield,str);
END runbutton_cb;

(*=====================*)
(* publics             *)
(*=====================*)

(*---------------------*)
PROCEDURE Init(str:TEXT) =
  CONST ftn = "Init";
  VAR
    argc : Xt.Cardinal := RTLinker.info.argc;
    argv : X.Argv      := RTLinker.info.argv;
  BEGIN
    debug(1,ftn,"begin\n");

    topLevel := Xt.AppInitialize(
                  app_context_return := appContext,
                  application_class  := M3toC.TtoS("Hello"),
                  options            := NIL,
                  num_options        := 0,
                  argc_in_out        := argc,
                  argv_in_out        := argv);

    XtVaSetValues(topLevel,
               XtN.x,IntVal(50),
               XtN.y,IntVal(50),
               XtN.width,IntVal(400),
               XtN.height,IntVal(200));
     
    MakeWidgets(str);
    Xt.RealizeWidget(topLevel);
    display:=Xt.Display(topLevel);
    
    Xt.AppMainLoop(appContext);    
  END Init;

(*-----------------*)
BEGIN
END HelloGUI.
