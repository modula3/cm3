INTERFACE Param;

PROCEDURE Switches(): CARDINAL;
(* Number of switches (command line parameters where first character is '-'. Switches are not counted after
   "--" is encountered on command line, and single '-' is not regarded as a switch, but as an ordinary argument.
*)

PROCEDURE Switch(opt: TEXT): BOOLEAN;
(* IF Switch('-i') THEN
     -i is present on command line, and happens before optional "--"
   ELSE
     -i is not present
   END;
*)

PROCEDURE GetSwitch(opt: TEXT; VAR res: TEXT): BOOLEAN;
(* Ako je na cmd liniji dat parametar '-o:output.txt' onda ce poziv
   "Params.GetSwitch('-o:', tStr)" vratiti TRUE, a u tStr ce biti 'output.txt'.

   Ovdje vazi i sve napisano za Switch proceduru.
*)

PROCEDURE Count(): CARDINAL;
(* Returns number of non-switch arguments on command line. First "--" is ignored, following ones are treated as arguments, if present.
*)

PROCEDURE Item(n: CARDINAL): TEXT;
(* Returns n-th argument (non-switch). For "--", see Count().
   Item(0) returns program name.
*)

END Param.
