(* $Id$ *)

INTERFACE Csighandler;

<*EXTERNAL Csighandler_have_signal*>
PROCEDURE have_signal() : INTEGER;

<*EXTERNAL Csighandler_clear_signal*>
PROCEDURE clear_signal();

<*EXTERNAL Csighandler_install_int_handler*>
PROCEDURE install_int_handler();

END Csighandler.
