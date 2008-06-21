(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE ConfigItem;

IMPORT IP;

TYPE
  T = {
    Verbose_log, Verbose_display, Max_display_items,
    Max_display_width, Max_display_columns,
    Use_multiple_windows, Refresh_interval, Auto_pkg_scan, Num_server_threads,
    Homepage, Server_port, Server_machine, IP_address, Start_browser, Build_package,
    Ship_package, Clean_package, Run_program, Edit_file
  };

  ItemDesc = RECORD
    name          : TEXT;
    id            : T;
    kind          : Kind;
  END;

  Kind = { Int, Bool, Text, Proc, IPAddr };

CONST
  Desc = ARRAY T OF ItemDesc {
    ItemDesc { "VERBOSE_LOG",          T.Verbose_log,          Kind.Bool   },
    ItemDesc { "VERBOSE_DISPLAY",      T.Verbose_display,      Kind.Bool   },
    ItemDesc { "MAX_DISPLAY_ITEMS",    T.Max_display_items,    Kind.Int    },
    ItemDesc { "MAX_DISPLAY_WIDTH",    T.Max_display_width,    Kind.Int    },
    ItemDesc { "MAX_DISPLAY_COLUMNS",  T.Max_display_columns,  Kind.Int    },
    ItemDesc { "USE_MULTIPLE_WINDOWS", T.Use_multiple_windows, Kind.Bool   },
    ItemDesc { "REFRESH_INTERVAL",     T.Refresh_interval,     Kind.Int    },
    ItemDesc { "AUTO_PKG_SCAN",        T.Auto_pkg_scan,        Kind.Bool   },
    ItemDesc { "NUM_SERVER_THREADS",   T.Num_server_threads,   Kind.Int    },
    ItemDesc { "HOMEPAGE",             T.Homepage,             Kind.Text   },
    ItemDesc { "SERVER_PORT",          T.Server_port,          Kind.Int    },
    ItemDesc { "SERVER_MACHINE",       T.Server_machine,       Kind.Text   },
    ItemDesc { "IP_ADDRESS",           T.IP_address,           Kind.IPAddr },
    ItemDesc { "start_browser",        T.Start_browser,        Kind.Proc   },
    ItemDesc { "build_package",        T.Build_package,        Kind.Proc   },
    ItemDesc { "ship_package",         T.Ship_package,         Kind.Proc   },
    ItemDesc { "clean_package",        T.Clean_package,        Kind.Proc   },
    ItemDesc { "run_program",          T.Run_program,          Kind.Proc   },
    ItemDesc { "edit_file",            T.Edit_file,            Kind.Proc   }
  };

TYPE
  Value = RECORD
    bool : BOOLEAN    := FALSE;
    int  : INTEGER    := 0;
    text : TEXT       := NIL;
    proc : TEXT       := NIL;
    addr : IP.Address := IP.NullAddress;
  END;

VAR
  X: ARRAY T OF Value;

PROCEDURE Set (t: T;  value: TEXT);

PROCEDURE SetExecutable (t: T;  value: TEXT);

PROCEDURE ToText (t: T): TEXT;

END ConfigItem.

