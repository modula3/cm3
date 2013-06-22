MODULE Main;

IMPORT IO,Fmt,Thread;
IMPORT QtObject;

IMPORT QtDynamic;

VAR
  ret : BOOLEAN;
  cb := NEW(QtObject.QObject).init_1();

  dyn := NEW(QtDynamic.DynamicQObject).init_0(Callback1,NIL);


PROCEDURE Callback1(obj : ADDRESS; args : ADDRESS) =
VAR
 i : INTEGER;
BEGIN

  IO.Put("In M3 callback ");
  (* signal emits no arguments but this is how we
     extract the parm
  i := QtDynamic.ConvertInt(args,1);
  IO.PutInt(i);
  *)
  IO.Put("\n");

END Callback1;

BEGIN

(*
   Very simple test to see if can connect. The only
   signal in QObject is destroyed. The slot1() parm below
   is just a name but must "look" like a C signature.
*)


  ret := dyn.connectDynamicSlot(cb, "destroyed()", "slot1()");
  IF ret THEN
    IO.Put("Slot connected\n");
  ELSE
    IO.Put("Slot not connected\n");
  END;

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

END Main.

