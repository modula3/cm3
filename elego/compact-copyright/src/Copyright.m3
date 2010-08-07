(*--------------------------------------------------------------------------*)
MODULE Copyright;

IMPORT Bundle;
IMPORT CopyrightBundle, SMsg AS Msg;

(*--------------------------------------------------------------------------*)
PROCEDURE Get(<*UNUSED*>t : T) : TEXT =
  VAR m : TEXT;
  BEGIN
    m := Bundle.Get(CopyrightBundle.Get(), "compact-copyrights.txt");
    RETURN m;
  END Get;

(*--------------------------------------------------------------------------*)
PROCEDURE Show(t : T) =
  VAR
    memo := Msg.tFlag;
    m    := Get(t);
  BEGIN
    Msg.tFlag := TRUE;
    Msg.T(m);
    Msg.tFlag := memo;
  END Show;

BEGIN
END Copyright.
