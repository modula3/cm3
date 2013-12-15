MODULE Main;

IMPORT IO,Fmt,Thread;
IMPORT QtObject;

IMPORT QtDynamic;

VAR
  ret : BOOLEAN;
  objcb := NEW(QtObject.QObject).init_1();

  dyn := NEW(QtDynamic.DynamicQObject).init_0(Callback1,NIL);

PROCEDURE Callback1(obj : ROOT; args : ADDRESS) =
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

  IF obj = NIL THEN
    IO.Put("obj is NIL\n");
  ELSE
    IO.Put("obj is not NIL\n");
  END;

END Callback1;

BEGIN

(*
   Very simple test to see if can connect. The only
   signal in QObject is destroyed(). The slot1() parm below
   is just a name but must "look" like a C signature.
*)

  ret := dyn.connectDynamicSlot(objcb, "destroyed()", "slot1()");
  IF ret THEN
    IO.Put("Object Slot connected\n");
  ELSE
    IO.Put("Object Slot not connected\n");
  END;

(* connecting twice should work *)

  ret := dyn.connectDynamicSlot(objcb, "destroyed()", "slot1()");
  IF ret THEN
    IO.Put("Object Slot connected\n");
  ELSE
    IO.Put("Object Slot not connected\n");
  END;

(* disconnent the slot - now it wont get the signal *)

  ret := dyn.disConnectDynamicSlot(objcb, "destroyed()", "slot1()");
  IF ret THEN
    IO.Put("Object Slot disconnected\n");
  ELSE
    IO.Put("Object Slot not disconnected\n");
  END;

(* disconnent the slot again and it should fail its not connected *)

  ret := dyn.disConnectDynamicSlot(objcb, "destroyed()", "slot1()");
  IF ret THEN
    IO.Put("Object Slot disconnected\n");
  ELSE
    IO.Put("Object Slot not disconnected\n");
  END;

(* fails since QObject does not have this signal *)

  ret := dyn.connectDynamicSlot(objcb, "nosuchsignal()", "slot1()");
  IF ret THEN
    IO.Put("Object Slot connected\n");
  ELSE
    IO.Put("Object Slot not connected\n");
  END;

(* this is wierd should fail since its not a valid C signature but
   often it succeeds *)

  ret := dyn.connectDynamicSlot(objcb, "destroyed()", "notasig");
  IF ret THEN
    IO.Put("Object Slot connected\n");
  ELSE
    IO.Put("Object Slot not connected\n");
  END;


  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

END Main.

