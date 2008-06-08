(*
 This program passes floats and doubles from Modula-3 to C code to check
 that they have the correct value. Strings are used as a presumed working
 transport.
*)

MODULE A EXPORTS Main;
FROM A IMPORT CheckF, CheckD;
IMPORT IO, Process;

BEGIN
(* NOTE every string here must be known by the C code.
We are testing float/double functionality and depend on string functionality. *)

    CheckF( 0.0, " 0.0");
    (* CheckF(-0.0, "-0.0"); *)
    CheckF( 1.0, " 1.0");
    CheckF(-1.0, "-1.0");
    CheckF( 0.5, " 0.5");
    CheckF(-0.5, "-0.5");
    CheckF( 1.5, " 1.5");
    CheckF(-1.5, "-1.5");

    CheckD( 0.0d0, " 0.0");
    (* CheckD(-0.0d0, "-0.0"); *)
    CheckD( 1.0d0, " 1.0");
    CheckD(-1.0d0, "-1.0");
    CheckD( 0.5d0, " 0.5");
    CheckD(-0.5d0, "-0.5");
    CheckD( 1.5d0, " 1.5");
    CheckD(-1.5d0, "-1.5");

    IO.Put("success\n");
    Process.Exit(0);

END A.
