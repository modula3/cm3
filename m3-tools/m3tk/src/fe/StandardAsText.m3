(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE StandardAsText;

IMPORT M3Conventions;

(* Typical C compilers dont like strings as long as this, so to avoid
   hassle, we build the TEXT at runtime (once). *)

PROCEDURE Get(): TEXT RAISES {}=
  BEGIN
    RETURN standard;
  END Get;


VAR standard: TEXT;

BEGIN
  standard :=  "INTERFACE " & M3Conventions.Standard & ";\n";

standard := standard & "TYPE\n";
standard := standard & "  CARDINAL = [0 .. LAST(INTEGER)];\n";
standard := standard & "  BOOLEAN = {FALSE, TRUE};\n";
standard := standard & "  CHAR = {NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL,\n";
standard := standard & "         BS, HT, NL, VT, NP, CR, SO, SI, \n";
standard := standard & "         DLE, DC1, DC2, DC3, DC4, NAK, SYN, ETB,\n";
standard := standard & "         CAN, EM, SUB, ESC, FS, GS, RS, US,\n";
standard := standard & "         SP, EXCLAMATION, DQUOTE, HASH, DOLLAR, PERCENT, AMPS, QUOTE,\n";
standard := standard & "         LPAREN, RPAREN, STAR, PLUS, COMMA, MINUS, DOT, SLASH,\n";
standard := standard & "         ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN,\n";
standard := standard & "         EIGHT, NINE, COLON, SEMICOLON, LT, EQ, GT, QUERY,\n";
standard := standard & "         AT, A, B, C, D, E, F, G,\n";
standard := standard & "         H, I, J, K, L, M, N, O,\n";
standard := standard & "         P, Q, R, S, T, U, V, W,\n";
standard := standard & "         X, Y, Z, LBRACKET, BACKSLASH, RBRACKET, HAT, UNDERLINE,\n";
standard := standard & "         BACKQUOTE, a, b, c, d, e, f, g,\n";
standard := standard & "         h, i, j, k, l, m, n, o,\n";
standard := standard & "         p, q, r, s, t, u, v, w,\n";
standard := standard & "         x, y, z, LBRACE, VBAR, RBRACE, TILDE, DEL,\n";
standard := standard & "          C200, C201, C202, C203, C204, C205, C206, C207,\n";
standard := standard & "          C210, C211, C212, C213, C214, C215, C216, C217,\n";
standard := standard & "          C220, C221, C222, C223, C224, C225, C226, C227,\n";
standard := standard & "          C230, C231, C232, C233, C234, C235, C236, C237,\n";
standard := standard & "          C240, C241, C242, C243, C244, C245, C246, C247,\n";
standard := standard & "          C250, C251, C252, C253, C254, C255, C256, C257,\n";
standard := standard & "          C260, C261, C262, C263, C264, C265, C266, C267,\n";
standard := standard & "          C270, C271, C272, C273, C274, C275, C276, C277,\n";
standard := standard & "          C300, C301, C302, C303, C304, C305, C306, C307,\n";
standard := standard & "          C310, C311, C312, C313, C314, C315, C316, C317,\n";
standard := standard & "          C320, C321, C322, C323, C324, C325, C326, C327,\n";
standard := standard & "          C330, C331, C332, C333, C334, C335, C336, C337,\n";
standard := standard & "          C340, C341, C342, C343, C344, C345, C346, C347,\n";
standard := standard & "          C350, C351, C352, C353, C354, C355, C356, C357,\n";
standard := standard & "          C360, C361, C362, C363, C364, C365, C366, C367,\n";
standard := standard & "          C370, C371, C372, C373, C374, C375, C376, C377};\n";

standard := standard & "  TEXT <: REFANY;\n";
standard := standard & "  MUTEX <: ROOT;\n";

standard := standard & "CONST\n";
standard := standard & "  FALSE = BOOLEAN.FALSE;  (* short forms *)\n";
standard := standard & "  TRUE = BOOLEAN.TRUE;\n";

standard := standard & "PROCEDURE ABS(x: ANY_TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE FLOAT(x: ANY_TYPE; t: M3TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE ROUND(x: ANY_TYPE): INTEGER;\n";

standard := standard & "PROCEDURE TRUNC(x: ANY_TYPE): INTEGER;\n";

standard := standard & "PROCEDURE FLOOR(x: ANY_TYPE): INTEGER;\n";

standard := standard & "PROCEDURE CEILING(x: ANY_TYPE): INTEGER;\n";

standard := standard & "PROCEDURE MAX(x, y: ANY_TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE MIN(x, y: ANY_TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE ORD(x: ANY_TYPE): INTEGER;\n";

standard := standard & "PROCEDURE VAL(e: INTEGER; t: M3TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE FIRST(x: ANY_TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE LAST(x: ANY_TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE NUMBER(x: ANY_TYPE): CARDINAL;\n";

standard := standard & "PROCEDURE TYPECODE(x: ANY_TYPE): INTEGER;\n";

standard := standard & "PROCEDURE ISTYPE(x: ANY_TYPE; t: M3TYPE): BOOLEAN;\n";

standard := standard & "PROCEDURE NARROW(x: ANY_TYPE; t: M3TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE BITSIZE(x: ANY_TYPE): CARDINAL;\n";

standard := standard & "PROCEDURE BYTESIZE(x: ANY_TYPE): CARDINAL;\n";

standard := standard & "PROCEDURE ADRSIZE(x: ANY_TYPE): CARDINAL;\n";

standard := standard & "PROCEDURE INC(VAR v: ANY_TYPE; n: INTEGER := 1);\n";

standard := standard & "PROCEDURE DEC(VAR v: ANY_TYPE; n: INTEGER := 1);\n";

standard := standard & "PROCEDURE NEW(t: M3TYPE; bindings: ANY_TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE SUBARRAY(a: ANY_TYPE; start, length: CARDINAL): ANY_TYPE;\n";

standard := standard & "(* Unsafe operations *)\n";

standard := standard & "PROCEDURE LOOPHOLE(x: ANY_TYPE; t: M3TYPE): ANY_TYPE;\n";

standard := standard & "PROCEDURE ADR(VAR x: ANY_TYPE): ADDRESS;\n";

standard := standard & "PROCEDURE DISPOSE(VAR x: ANY_TYPE);\n";

standard := standard & "END " & M3Conventions.Standard & ".\n";


END StandardAsText.

