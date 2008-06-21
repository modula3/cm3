INTERFACE Default;

IMPORT Quake, QValue, Thread;
IMPORT ConfigItem;

VAR (* host dependent configuration *)
  on_unix         : BOOLEAN;
  slash           : TEXT;  (* separates arcs in a path *)
  path_sep        : CHAR;  (* separates entries in a path list *)

VAR (* cm3 configuration *)
  build_dir       : TEXT;  (* == BUILD_DIR from cm3.cfg *)
  system_root     : TEXT;  (* == PKG_USE from cm3.cfg *)
  doc_root        : TEXT;  (* == DOC_INSTALL from cm3.cfg *)
  example_root    : TEXT;  (* == INSTALL_ROOT/examples from cm3.cfg *)
  initial_browser : TEXT;  (* == INITIAL_CM3_IDE_BROWSER from cm3.cfg *)
  initial_editor  : TEXT;  (* == INITIAL_CM3_IDE_EDITOR from cm3.cfg *)

VAR (* CM3-IDE configuration *)
  server_href     : TEXT;
  user_home       : TEXT;  (* directory containing config state & local pkgs *)

PROCEDURE Init ();

PROCEDURE GetConfigProc (ci   : ConfigItem.T;
              VAR(*OUT*) m    : Quake.Machine;
              VAR(*OUT*) proc : QValue.T)
              RAISES {Thread.Alerted};

END Default.
