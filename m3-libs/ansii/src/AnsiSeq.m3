MODULE AnsiSeq;

(*
Colour Codes for use with SGR
FG      BG      Colour
30	40	Black
31	41	Red
32	42	Green
33	43	Yellow
34	44	Blue
35	45	Magenta
36	46	Cyan
37	47	White
90	100	Bright Black
91	101	Bright Red
92	102	Bright Green
93	103	Bright Yellow
94	104	Bright Blue
95	105	Bright Magenta
96	106	Bright Cyan
97	107	Bright White
*)

IMPORT Fmt;

(* cursor up *)
PROCEDURE CUU(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "A";
  END CUU;

(* cursor down *)
PROCEDURE CUD(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "B";
  END CUD;

(* cursor forward *)
PROCEDURE CUF(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "C";
  END CUF;

(* cursor back *)
PROCEDURE CUB(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "D";
  END CUB;

(* cursor next line *)
PROCEDURE CNL(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "E";
  END CNL;

(* cursor previous line *)
PROCEDURE CPL(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "F";
  END CPL;

(* cursor horizontal absolute *)
PROCEDURE CHA(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "G";
  END CHA;

(* cursor position *)
PROCEDURE CUP(n,m : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & ";" & Fmt.Int(m) & "H";
  END CUP;

(* erase in display *)
PROCEDURE ED(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "J";
  END ED;

(* erase in line *)
PROCEDURE EL(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "K";
  END EL;

(* scroll up *)
PROCEDURE SU(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "S";
  END SU;

(* scroll down *)
PROCEDURE SD(n : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & "T";
  END SD;

(* horizontal vertical position *)
PROCEDURE HVP(n,m : CARDINAL) : TEXT =
  BEGIN
    RETURN CSI & Fmt.Int(n) & ";" & Fmt.Int(m) & "f";
  END HVP;

(* select graphics rendition *)
PROCEDURE SGR(READONLY p : ARRAY OF CARDINAL) : TEXT =
  VAR ret := "";
  BEGIN
    FOR i := FIRST(p) TO LAST(p) DO
       ret := ret & Fmt.Int(p[i]);
       IF i # LAST(p) THEN ret := ret & ";" END;
    END;
    RETURN CSI & ret & "m";
  END SGR;

(* device status report *)
(* the device will send back ESC[n;mR where n is row and m is column.
   to read it you have to make stdin non-blocking and use Scan to extract parms *)
PROCEDURE DSR() : TEXT =
  BEGIN
    RETURN CSI & "6n";
  END DSR;

(* save current cursor position *)
PROCEDURE SCP() : TEXT =
  BEGIN
    RETURN CSI & "s";
  END SCP;

(* restore saved cursor position *)
PROCEDURE RCP() : TEXT =
  BEGIN
    RETURN CSI & "u";
  END RCP;

(* show cursor *)
PROCEDURE SC() : TEXT =
  BEGIN
    RETURN CSI & "h";
  END SC;

(* hide cursor *)
PROCEDURE HC() : TEXT =
  BEGIN
    RETURN CSI & "l";
  END HC;

BEGIN
END AnsiSeq.

