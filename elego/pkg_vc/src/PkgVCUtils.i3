(*---------------------------------------------------------------------------*)
INTERFACE PkgVCUtils;

IMPORT MsgIF, TextTextTbl;

(*---------------------------------------------------------------------------*)
EXCEPTION E(TEXT);

(*---------------------------------------------------------------------------*)
PROCEDURE GetCommitMessage(editor : TEXT; msgif : MsgIF.T := NIL; 
                           desc := ""; pkg := "<unknown>") : TEXT;

(*---------------------------------------------------------------------------*)
PROCEDURE GetMessage(editor : TEXT; msgif : MsgIF.T := NIL; msg : TEXT;
                     failIfUnchanged := TRUE) : TEXT;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckCommitMsg(msg, msgFileName, pkgName, pkgRoot, user, 
                         repository, action, name : TEXT; env : TextTextTbl.T)
  RAISES {E};
  (* Check a commit message and raise an exception if any flaw is found.
     Either `msg' or `msgFileName' must be specified (non-NIL). `pkgName' is
     the name of the package (or project) to be committed, `pkgRoot' is the
     root directory of the version controlled package structure, `user' the
     logname of the current user, `repository' the CVS repository identifier,
     `action' one of package-commit, package-release, project-snapshot,
     project-release, or project-change-set, and `name' the name of the new
     revision, snapshot, release, or change set. `env' is the internal
     environment which is searched for the following commit hooks:
     external-package-release-hook (for package releases),
     external-package-commit-hook (for new package development versions),
     external-project-snapshot-hook (for project snapshots),
     external-project-release-hook (for project releases),
     external-project-change-set-hook (for project commits creating change
     sets), and external-commit-hook (if no other appropriate hook is
     defined). 
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE MsgWithoutPkgInfoLines(fmsg : TEXT; msgif : MsgIF.T := NIL) : TEXT;
  (* Remove lines beginning with `PKG:' from fmsg *)

END PkgVCUtils.
