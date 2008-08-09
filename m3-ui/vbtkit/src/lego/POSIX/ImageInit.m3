MODULE ImageInit;

(*
Historically Image.Raw.xres and Image.Raw.yres were both always 75.0.
Historically that was not too far from what monitors had.
These days (2008) monitors can have much higher dpi
and the values are computed.
*)

PROCEDURE GetDefaultXRes() : REAL =
BEGIN
  RETURN 75.0;
END GetDefaultXRes;

PROCEDURE GetDefaultYRes() : REAL =
BEGIN
  RETURN 75.0;
END GetDefaultYRes;

BEGIN
END ImageInit.
