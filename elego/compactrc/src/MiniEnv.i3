(*--------------------------------------------------------------------------*)
INTERFACE MiniEnv;

VAR
  isPosix      := FALSE;
  isWin32      := FALSE;
  home         := "/tmp";
  user         := "dummy";
  compactroot  := "/usr/local/lib/compact";
  tmpdir       := "/tmp";
  tpc_hosttype := "";
  tpc_ostype   := "";
  tpc_variant  := "";
  tpc_compiler := "";
  tpc_options  := "";
  pass         := "not_a_valid_passphrase";
  editor       := "xterm -e vi";
  editorovr    : TEXT := NIL;
  httpdeditorovr : TEXT := NIL;
  editorArgsPathStyle : CHAR := 'n';
  (* n = native editor arguments path style, 
     p = posix (forward slashes), 
     w = windows (backslashes *)
END MiniEnv.
