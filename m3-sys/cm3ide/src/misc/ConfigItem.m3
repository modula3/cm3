(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE ConfigItem;

IMPORT Fmt, IO, IP, Params, Pathname, Text, Thread, Wr;
IMPORT Default, ErrLog, LexMisc, OS, Text2, UserState, Wx;

TYPE
  ItemDefault = RECORD
    id            : T;
    lo, hi        : INTEGER;
    default_int   : INTEGER;
    default_txt   : TEXT := NIL;
    proc_head     : TEXT := NIL;
    unix_middle   : TEXT := NIL;
    win32_middle  : TEXT := NIL;
    cm3           : TEXT := NIL;
    proc_tail     : TEXT := NIL;
    prog_name     : TEXT := NIL;
  END;

CONST
  BROWSER = "web browser";
  EDITOR  = "text editor";

CONST
  Defaults = ARRAY T OF ItemDefault {
    ItemDefault { T.Verbose_log,           0,      1,    0 },
    ItemDefault { T.Verbose_display,       0,      1,    0 },
    ItemDefault { T.Max_display_items,    30, 999999,   75 },
    ItemDefault { T.Max_display_width,    10, 999999,   70 },
    ItemDefault { T.Max_display_columns,   1, 999999,    5 },
    ItemDefault { T.Use_multiple_windows,  0,      1,    0 },
    ItemDefault { T.Refresh_interval,      1, 999999,   30 },
    ItemDefault { T.Auto_pkg_scan,         0,      1,    1 },
    ItemDefault { T.Num_server_threads,    1,     99,    3 },
    ItemDefault { T.Homepage,              0,      0,    0 },
    ItemDefault { T.Server_port,           0, 999999, 3800 },
    ItemDefault { T.Server_machine,        0,      0,    0, "localhost" },
    ItemDefault { T.IP_address,            0,      0,    0 },

    ItemDefault { T.Start_browser, 0, 0, 0, NIL,
        "proc start_browser (initial_url) is\n"
      & "  cm3_exec (\"",     "", "start /wait ", NIL, "\", initial_url)\n"
      & "  return TRUE %==> server terminates when browser terminates\n"
      & "end\n",
      BROWSER
    },

    ItemDefault { T.Build_package, 0, 0, 0, NIL,
        "proc build_package (pkg, options) is\n"
      & "  cm3_exec (\"cd\", pkg, \"",  "; ", "&& ", "cm3", "\", options)\n"
      & "end\n"
    },

    ItemDefault { T.Ship_package, 0, 0, 0, NIL,
        "proc ship_package (pkg) is\n"
      & "  cm3_exec (\"cd\", pkg, \"", "; ", "&& ", "cm3", " -ship\")\n"
      & "end\n"
    },

    ItemDefault { T.Clean_package, 0, 0, 0, NIL,
        "proc clean_package (pkg) is\n"
      & "  cm3_exec (\"cd\", pkg, \"", "; ", "&& ", "cm3", " -clean\")\n"
      & "end\n"
    },

    ItemDefault { T.Run_program, 0, 0, 0, NIL,
        "proc run_program (dir, cmd) is\n"
      & "  cm3_exec (\"cd\", dir, \"", "; ", "&& ", NIL, "\", cmd)\n"
      & "end\n"
    },

    ItemDefault { T.Edit_file, 0, 0, 0, NIL,
        "proc edit_file (file, line) is\n"
      & "  cm3_exec (\"", "", "", NIL, "\", \"+\" & line, file)\n"
      & "end\n",
      EDITOR
    }

  };

PROCEDURE Set (t: T;  value: TEXT) =
  BEGIN
    WITH desc = Desc[t], val = X[t] DO
      CASE desc.kind OF
      | Kind.Bool   =>  val.bool := SetBool (desc, value);
      | Kind.Int    =>  val.int  := SetInt (desc, value);
      | Kind.Text   =>  val.text := SetText (desc, value);
      | Kind.Proc   =>  val.proc := SetProc (desc, value);
      | Kind.IPAddr =>  val.addr := SetIPAddr (value);
      END;
      UserState.Put (desc.name, ToText (t));
    END;
  END Set;

PROCEDURE SetExecutable (t: T;  value: TEXT) =
  BEGIN
    WITH desc = Desc[t], val = X[t] DO
      <*ASSERT desc.kind = Kind.Proc*>
      val.proc := BuildProc (desc, value);
      UserState.Put (desc.name, ToText (t));
    END;
  END SetExecutable;

PROCEDURE SetBool (READONLY desc: ItemDesc;  txt: TEXT): BOOLEAN =
  CONST Map = ARRAY BOOLEAN OF TEXT { "FALSE", "TRUE" };
  VAR val := VAL (Defaults[desc.id].default_int, BOOLEAN);
  BEGIN
    IF (txt = NIL) THEN
      (* use default *)
    ELSIF Text.Equal (txt, Map[FALSE]) THEN
      val := FALSE;
    ELSIF Text.Equal (txt, Map[TRUE]) THEN
      val := TRUE;
    ELSE
      (* use default *)
      ErrLog.Msg ("Unrecognized boolean value (\"", txt, "\") for \"",
                   desc.name & "\", using \""
                   & Map[val] & "\" instead");
    END;
    RETURN val;
  END SetBool;

PROCEDURE SetInt (READONLY desc: ItemDesc;  txt: TEXT): INTEGER =
  VAR
    val := Defaults[desc.id].default_int;
    lo  := Defaults[desc.id].lo;
    hi  := Defaults[desc.id].hi;
  BEGIN
    IF (txt # NIL) AND Text.Length (txt) > 0 THEN
      val := LexMisc.ScanInt (txt);
      IF (val < lo) THEN
        val := lo;
        ErrLog.Msg ("Value specified for ", desc.name, "(", txt & ") is too small, "
                    & Fmt.Int (val) & " used instead.");
      ELSIF (hi < val) THEN
        val := hi;
        ErrLog.Msg ("Value specified for ", desc.name, "(", txt & ") is too big, "
                    & Fmt.Int (val) & " used instead.");
      END;
    END;
    RETURN val;
  END SetInt;

PROCEDURE SetText (READONLY desc: ItemDesc;  txt: TEXT): TEXT =
  BEGIN
    IF (txt = NIL) THEN
      txt := Defaults[desc.id].default_txt;
    END;
    RETURN txt;
  END SetText;

PROCEDURE SetProc (READONLY desc: ItemDesc;  txt: TEXT): TEXT =
  BEGIN
    IF (txt = NIL) OR Text.Length (txt) <= 0 THEN
      txt := BuildProc (desc, NIL);
    END;
    RETURN txt;
  END SetProc;

PROCEDURE BuildProc (READONLY desc: ItemDesc;  prog: TEXT): TEXT =
  VAR mid: TEXT;
  BEGIN
    WITH z = Defaults[desc.id] DO
      IF (z.prog_name # NIL) THEN  prog := GetProg (z.prog_name, prog);  END;
      IF (prog = NIL) THEN prog := ""; END;
      IF Default.on_unix
        THEN mid := z.unix_middle;
        ELSE mid := z.win32_middle;
      END;
      RETURN z.proc_head & mid & FindCm3 (z.cm3) & prog & z.proc_tail;
    END;
  END BuildProc;

VAR cm3_exe: TEXT := NIL;

PROCEDURE FindCm3 (cm3: TEXT): TEXT =
  BEGIN
    IF (cm3 = NIL) THEN (* => not needed *)  RETURN ""; END;
    IF cm3_exe = NIL THEN cm3_exe := Cm3Location (cm3); END;
    RETURN cm3_exe;
  END FindCm3;

PROCEDURE Cm3Location (cm3: TEXT): TEXT =
  VAR exe: TEXT;
  BEGIN
    exe := OS.FindExecutable (cm3);
    IF exe # NIL THEN
      (* "cm3" plus the the existing $PATH is good enough. *)
      RETURN cm3;
    END;

    (* hmmm, try the directory containing CM3-IDE *)
    exe := Pathname.Join (Pathname.Prefix (Params.Get (0)), cm3, NIL);
    exe := OS.FindExecutable (exe);
    IF (exe # NIL) THEN
      (* we found one! *)
      RETURN exe;
    END;

    (* Nope, just use the default *)
    RETURN cm3;
  END Cm3Location;

PROCEDURE GetProg (nm: TEXT;  default: TEXT): TEXT =
  VAR prog, exe: TEXT;
  BEGIN
    IF (default # NIL) THEN
      exe := FindProg (default);
      IF (exe # NIL) THEN
        IF NOT Text.Equal (default, exe) THEN
          Out ("Using \"", exe, "\" for your ", nm, ".");
        END;
        RETURN Text2.Escape (Text2.FixExeName (exe));
      END;
    ELSIF Text.Equal (nm, BROWSER) THEN
      exe := FindProg (Default.initial_browser);
      IF (exe # NIL) THEN
        Out ("Using \"", exe, "\" for your ", nm, ".");
        RETURN Text2.Escape (Text2.FixExeName (exe));
      END;
    ELSIF Text.Equal (nm, EDITOR) THEN
      exe := FindProg (Default.initial_editor);
      IF (exe # NIL) THEN
        Out ("Using \"", exe, "\" for your ", nm, ".");
        RETURN Text2.Escape (Text2.FixExeName (exe));
      END;
    END;

    LOOP
      Out ("What program should CM3-IDE use for your ", nm, "? ");
      TRY
        prog := Text2.Trim (IO.GetLine ());
        exe := FindProg (prog);
        IF (exe # NIL) THEN
          IF NOT Text.Equal (exe, prog) THEN
            Out ("... using: ", exe);
          END;
          RETURN Text2.Escape (Text2.FixExeName (exe));
        END;
        Out ("\"", prog, "\" is not an executable file.");
      EXCEPT IO.Error =>
        Out ("huh?");
      END;
    END;
  END GetProg;

PROCEDURE FindProg (nm: TEXT): TEXT =
  BEGIN
    IF (nm = NIL) THEN RETURN NIL; END;
    nm := Text2.Trim (nm);
    IF Text.Length (nm) > 0
      THEN RETURN OS.FindExecutable (nm);
      ELSE RETURN NIL;
    END;
  END FindProg;

PROCEDURE SetIPAddr (txt: TEXT): IP.Address =
  VAR addr: IP.Address;
  BEGIN
    IF (txt = NIL) OR (Text.Length (txt) <= 0) THEN
      addr := IP.NullAddress;
    ELSIF NOT LexMisc.ScanIPAddress (txt, addr) THEN
      ErrLog.Msg ("improperly formatted IP address: \"", txt, "\", using 0.0.0.0");
      addr := IP.NullAddress;
    END;
    RETURN addr;
  END SetIPAddr;

PROCEDURE ToText (t: T): TEXT =
  BEGIN
    CASE Desc[t].kind OF
    | Kind.Bool   =>  RETURN Fmt.Bool (X[t].bool);
    | Kind.Int    =>  RETURN Fmt.Int (X[t].int);
    | Kind.Text   =>  RETURN X[t].text;
    | Kind.Proc   =>  RETURN X[t].proc;
    | Kind.IPAddr =>
        IF X[t].text = NIL THEN  X[t].text := FmtIPAddr (X[t].addr);  END;
        RETURN X[t].text;
    END;
  END ToText;

PROCEDURE FmtIPAddr (READONLY addr: IP.Address): TEXT =
  <*FATAL Wr.Failure, Thread.Alerted *>
  VAR wx: Wx.T;
  BEGIN
    IF addr = IP.NullAddress THEN RETURN ""; END;
    wx := NEW (Wx.T).init (NIL);
    wx.putInt (addr.a[0]);
    wx.put (".");
    wx.putInt (addr.a[1]);
    wx.put (".");
    wx.putInt (addr.a[2]);
    wx.put (".");
    wx.putInt (addr.a[3]);
    RETURN wx.toText ();
  END FmtIPAddr;

PROCEDURE Out (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN IO.Put (a); END;
    IF (b # NIL) THEN IO.Put (b); END;
    IF (c # NIL) THEN IO.Put (c); END;
    IF (d # NIL) THEN IO.Put (d); END;
    IF (e # NIL) THEN IO.Put (e); END;
    IO.Put (Wr.EOL);
  END Out;

BEGIN
  FOR t := FIRST (Desc) TO LAST (Desc) DO
    <* ASSERT Desc[t].id = t *>
    <* ASSERT Defaults[t].id = t *>
  END;
END ConfigItem.

