(*--------------------------------------------------------------------------*)
INTERFACE CompactRC;

IMPORT Rd, TextTextTbl, Bundle, TextList, Rsrc;
IMPORT TextTupleSeq, TextTextTupleSeqTbl, MsgIF, SimpleValueEnv;

(*--------------------------------------------------------------------------*)
TYPE
  TpcOverrides = OBJECT
    hosttype  : TEXT; (* override from parameters, default = NIL *)
    ostype    : TEXT; (* override from parameters, default = NIL *)
    variant   : TEXT; (* override from parameters, default = NIL *)
    compiler  : TEXT; (* override from parameters, default = NIL *)
    options   : TEXT; (* override from parameters, default = NIL *)
  METHODS
    init() : TpcOverrides := TpcOverridesInit;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE TpcOverridesInit(self : TpcOverrides) : TpcOverrides;

(*--------------------------------------------------------------------------*)
VAR mappingLists : TextTextTupleSeqTbl.T;
  (* This is an associate array of mapping lists, e.g. named mappings.
     It is filled with mappings for all keywords that end in `-mapping', 
     as they are encountered in reading the configuration file.
   *)

(*--------------------------------------------------------------------------*)
PROCEDURE MappingDefined(name : TEXT) : BOOLEAN;
  (* <=> There was at least one `name => value' line for mapping `name'
     in the configuration file. *)

(*--------------------------------------------------------------------------*)
PROCEDURE GetMapping(name : TEXT) : TextTupleSeq.T;
  (* Return the mapping `name' if defined, NIL otherwise. *)
  
(*--------------------------------------------------------------------------*)
PROCEDURE SetSomeDefaults(env : TextTextTbl.T);
  (* Define the following values in table `env':
     HOME, USER, COMPACTROOT, TMPDIR, editor, httpd-editor
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE SetEnvVars(env : TextTextTbl.T);
  (* Define the following values if they are set in the process
     environment: 
     PRJ_ROOT, COLLECTIONROOT, PROJECTROOT,
     PKG_USE_EXTERNAL_MKDIR, PKG_USE_POSIX_PATHNAMES
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE SetTPCDefaults(env : TextTextTbl.T;
                         tpcOverrides : TpcOverrides := NIL;
                         tpcWarnings  : BOOLEAN := FALSE; 
                         msg : MsgIF.T := NIL;
                         warn := TRUE);
  (* Defines the variables tpc-hosttype, tpc-ostype, tpc-variant,
     tpc-compiler, tpc-options either from tpc-overrides or from 
     defaults in MiniEnv. These latter are only used if no other
     definition is present. 
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE Eval(initBundle   : Bundle.T; 
               tpcOverrides : TpcOverrides := NIL;
               tpcWarnings  : BOOLEAN := FALSE;
               readEnvFile  : BOOLEAN := TRUE;
               msg : MsgIF.T := NIL) : TextTextTbl.T;
  (* Evaluate the contents of the central ComPact configuration file,
     which is either $(HOME)/.compactrc, or $(HOME)/compact/compactrc, or
     /usr/contrib/lib/compact/compactrc, or /usr/local/lib/compact/compactrc,
     or /opt/compact/compactrc, or contained in `initBundle'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE ReadAll(env          : TextTextTbl.T;
                  rsrcPath     : Rsrc.Path;
                  initBundle   : Bundle.T; 
                  tpcOverrides : TpcOverrides := NIL;
                  tpcWarnings  : BOOLEAN := FALSE; 
                  msg : MsgIF.T := NIL);
  (* Check and read all possible compactrc resources: bundled, from 
     remote service, local resource located via resource path, and
     ~/.compactrc* files. Evaluate TPC settings using SetTPCDefaults.
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE Read(rd : Rd.T; env : TextTextTbl.T; msg : MsgIF.T := NIL);
  (* Read and evaluate the contents of a ComPact initializaion file. *)

(*--------------------------------------------------------------------------*)
PROCEDURE EnvFileLocation(home : TEXT) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE ReadEnvFile(fn : TEXT; env : TextTextTbl.T; sve : SimpleValueEnv.T;
                      msg : MsgIF.T := NIL) RAISES {SimpleValueEnv.E};
  (* Read an environment file created by SimpleValueEnv.Write() and
     add all contents that can be converted to text to `env'.
     Issue warnings if any old settings are overwritten. 
     Return the complete environment in `sve'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE WriteEnvFile(fn : TEXT := NIL; sve : SimpleValueEnv.T;
                       keys : TextList.T := NIL; rec := FALSE)
  RAISES {SimpleValueEnv.E};
  (* Write `sve' to file `fn'. If fn is NIL, determine an appropriate
     default path (~/.compact-env-rc or (~/compact/compact-env-rc). *)

(*--------------------------------------------------------------------------*)
PROCEDURE ComputePkgKind(env : TextTextTbl.T; msg : MsgIF.T := NIL) : TEXT;
  (* Returns the package kind the user wants to use by considering the
     process environment, the environment read from the compactrc file, 
     and the mappings defined therein. *)

(*--------------------------------------------------------------------------*)
PROCEDURE ComputeTPC(env : TextTextTbl.T; msg : MsgIF.T := NIL) : TEXT;
  (* Returns the TPC the user wants to use by considering the
     process environment, the environment read from the compactrc file, 
     and the mappings defined therein. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Defined(env : TextTextTbl.T; name : TEXT) : BOOLEAN;
  (* <=> The value `name' is defined in `env'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Get(env : TextTextTbl.T; name : TEXT) : TEXT;
  (* Returns the mapping of `name' from `env', if it is defined, NIL else. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Put(env : TextTextTbl.T; name : TEXT; val : TEXT);
  (* Defines the mapping `name' --> `val' in `env'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE GetValue(env : TextTextTbl.T; name : TEXT; msg : MsgIF.T := NIL)
  : TEXT;
  (* Returns the value of `name' from the environement `env', if it
     is defined, NIL else. The value is the mapping from the environment
     after substitution of all process environment variables, 
     internal environment variables, and the special variables
     {HOME} and {USER}. 

     The procedures TextUtils.SubstEnvVars() and 
     TextUtils.SubstituteVariables() are used to perform the actual
     substitutions, which means that environment variables of the
     form $name and ${name} are recognized and internal variable
     names of the form {:name}, {!name}, and {?name}.
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE Evaluate(env: TextTextTbl.T; msg : MsgIF.T := NIL) : TextTextTbl.T;
  (* Returns a new environment containing the same variables as `env',
     but with all variable references within their values resolved.
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE GetEditorCmd(env : SimpleValueEnv.T; 
                       files : TEXT; msg : MsgIF.T := NIL) : TEXT;

  (* Choose an editor based on the httpd-editor-EXT values in env or
     the filename-editor-mapping; fall back to httpd-editor or
     editor if all else fails *)

(*--------------------------------------------------------------------------*)
PROCEDURE GetRsrcText(name : TEXT; bundle : Bundle.T; rsrcPath : Rsrc.Path;
                      env  : TextTextTbl.T := NIL;
                      senv : SimpleValueEnv.T := NIL;
                      commentStart := "# ";
                      msg  : MsgIF.T := NIL) : TEXT;
  (* Read all available resources named `name' and concatenate them.
     Use `commentStart' to begin numbered separator lines if it is
     non-NIL. The bundle is always used first, then a remote service
     is contacted. Last of all the rsrcPath is used to locate
     resources in the local file system. *)

(*--------------------------------------------------------------------------*)
PROCEDURE GetSingleRsrcText(name : TEXT; rsrcPath : Rsrc.Path;
                            bundlePath : Rsrc.Path;
                            env  : TextTextTbl.T := NIL;
                            senv : SimpleValueEnv.T := NIL;
                            msg  : MsgIF.T := NIL) : TEXT;
  (* Get exactly one version of resource `name' as text. First check
     for local resources (rsrcPath), then try the remote service, last
     use the bundled resources (bundlePath). If none is found, return
     NIL. *)

END CompactRC.
