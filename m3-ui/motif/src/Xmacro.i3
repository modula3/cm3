(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:45:38 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE INTERFACE Xmacro;

IMPORT Xt;
FROM Ctypes IMPORT 
  char_star,short,unsigned_short;

<*EXTERNAL"XtVaSetValues"*>
PROCEDURE XtVaSetValues(widget:Xt.Widget;
		 val1, val2, val3, val4, val5,
		 val6, val7, val8, val9, val10,
		 val11,val12,val13,val14,val15,
		 val16:ADDRESS:=NIL);

<*EXTERNAL"XtVaGetValues"*>
PROCEDURE XtVaGetValues(widget:Xt.Widget;
		 val1, val2, val3, val4, val5,
		 val6, val7, val8, val9, val10,
		 val11,val12,val13,val14,val15,
		 val16:ADDRESS:=NIL);

<*EXTERNAL "XtVaCreateManagedWidget"*>
PROCEDURE XtVaCreateManagedWidget(name:char_star;
	         widget_class: Xt.WidgetClass;
		 parent: Xt.Widget;
		 val1, val2, val3, val4, val5,
		 val6, val7, val8, val9, val10,
		 val11,val12,val13,val14,val15,
		 val16:ADDRESS:=NIL):Xt.Widget;


PROCEDURE AddrVal(value:ADDRESS):Xt.ArgVal;
PROCEDURE CharVal(value:CHAR):Xt.ArgVal;
PROCEDURE IntVal(value:INTEGER):Xt.ArgVal;
PROCEDURE ShortVal(value:short):Xt.ArgVal;
PROCEDURE TextVal(value:TEXT):Xt.ArgVal;
PROCEDURE UShortVal(value:unsigned_short):Xt.ArgVal;

PROCEDURE XtNewString(str:char_star):char_star;

END Xmacro.
