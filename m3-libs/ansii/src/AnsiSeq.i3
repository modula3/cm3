(* A module to print ansii escape sequences to a terminal, thus allowing
   coloured text and navigation.
*)

INTERFACE AnsiSeq;

CONST

  (* control sequence introducer *)
  CSI = "\033[";

  BELL = "\X07";

  (*Reset*)
  RESET = CSI & "0m";
  (* reverse *)
  REVERSE = CSI & "7m";

  (* Constant shortcuts for common colours *)

  (*Regular text*)
  BLK = CSI & "0;30m";
  RED = CSI & "0;31m";
  GRN = CSI & "0;32m";
  YEL = CSI & "0;33m";
  BLU = CSI & "0;34m";
  MAG = CSI & "0;35m";
  CYN = CSI & "0;36m";
  WHT = CSI & "0;37m";

  (*Regular bold text*)
  BBLK = CSI & "1;30m";
  BRED = CSI & "1;31m";
  BGRN = CSI & "1;32m";
  BYEL = CSI & "1;33m";
  BBLU = CSI & "1;34m";
  BMAG = CSI & "1;35m";
  BCYN = CSI & "1;36m";
  BWHT = CSI & "1;37m";

  (*Regular underline text*)
  UBLK = CSI & "4;30m";
  URED = CSI & "4;31m";
  UGRN = CSI & "4;32m";
  UYEL = CSI & "4;33m";
  UBLU = CSI & "4;34m";
  UMAG = CSI & "4;35m";
  UCYN = CSI & "4;36m";
  UWHT = CSI & "4;37m";

  (*Regular background*)
  BLKB = CSI & "40m";
  REDB = CSI & "41m";
  GRNB = CSI & "42m";
  YELB = CSI & "43m";
  BLUB = CSI & "44m";
  MAGB = CSI & "45m";
  CYNB = CSI & "46m";
  WHTB = CSI & "47m";

  (*High intensty background*)
  BLKHB = CSI & "0;100m";
  REDHB = CSI & "0;101m";
  GRNHB = CSI & "0;102m";
  YELHB = CSI & "0;103m";
  BLUHB = CSI & "0;104m";
  MAGHB = CSI & "0;105m";
  CYNHB = CSI & "0;106m";
  WHTHB = CSI & "0;107m";

  (*High intensty text*)
  HBLK = CSI & "0;90m";
  HRED = CSI & "0;91m";
  HGRN = CSI & "0;92m";
  HYEL = CSI & "0;93m";
  HBLU = CSI & "0;94m";
  HMAG = CSI & "0;95m";
  HCYN = CSI & "0;96m";
  HWHT = CSI & "0;97m";

  (*Bold high intensity text*)
  BHBLK = CSI & "1;90m";
  BHRED = CSI & "1;91m";
  BHGRN = CSI & "1;92m";
  BHYEL = CSI & "1;93m";
  BHBLU = CSI & "1;94m";
  BHMAG = CSI & "1;95m";
  BHCYN = CSI & "1;96m";
  BHWHT = CSI & "1;97m";


(* cursor up *)
PROCEDURE CUU(n : CARDINAL) : TEXT;

(* cursor down *)
PROCEDURE CUD(n : CARDINAL) : TEXT;

(* cursor forward *)
PROCEDURE CUF(n : CARDINAL) : TEXT;

(* cursor back *)
PROCEDURE CUB(n : CARDINAL) : TEXT;

(* cursor next line *)
PROCEDURE CNL(n : CARDINAL) : TEXT;

(* cursor previous line *)
PROCEDURE CPL(n : CARDINAL) : TEXT;

(* cursor horizontal absolute *)
PROCEDURE CHA(n : CARDINAL) : TEXT;

(* cursor position *)
PROCEDURE CUP(n,m : CARDINAL) : TEXT;

(* erase in display *)
PROCEDURE ED(n : CARDINAL) : TEXT;

(* erase in line *)
PROCEDURE EL(n : CARDINAL) : TEXT;

(* scroll up *)
PROCEDURE SU(n : CARDINAL) : TEXT;

(* scroll down *)
PROCEDURE SD(n : CARDINAL) : TEXT;

(* horizontal vertical position *)
PROCEDURE HVP(n,m : CARDINAL) : TEXT;

(* select graphics rendition *)
PROCEDURE SGR(READONLY p : ARRAY OF CARDINAL) : TEXT;

(* device status report *)
(* the device will send back ESC[n;mR where n is row and m is column.
   to read it you have to make stdin non-blocking and use Scan to extract parms *)
PROCEDURE DSR() : TEXT;

(* save current cursor position *)
PROCEDURE SCP() : TEXT;

(* restore saved cursor position *)
PROCEDURE RCP() : TEXT;

(* show cursor *)
PROCEDURE SC() : TEXT;

(* hide cursor *)
PROCEDURE HC() : TEXT;

END AnsiSeq.
